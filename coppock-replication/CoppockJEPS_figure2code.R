#### Coppock Replication code for JEPS -- FIGURE 2
#### Butler, Daniel M., and David W. Nickerson. 2011. 
#### “Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment.” 
#### Quarterly Journal of Political Science 6(1): 55–83. DOI: 10.1561/100.00011019

rm(list=ls())
# Set your working directory
# setwd("")

library(lattice)

find_breaks <- function(x){
  breaks <- rep(NA, length(x)-1)
  for(i in 1:length(breaks)){
    breaks[i+1] <- x[i]!=x[i+1]
  }
  return(which(breaks))
}

load("CoppockJEPS.rdata")

## Pick exposure model -- main analysis uses dw nominate
Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
exposure.obs <- CoppockJEPS$s.difs.dw

directs <-seq(from=-.7, to=0.2, by=.025)
indirects <-seq(from=-.7, to=0.2, by=.025)

sims <- 500
pmat.ssr <- matrix(NA, length(directs), length(indirects))

# write a function to compute exposure as a function of a random treatment variable Z
exposure <- function(Z) {
  exposure.observed.dw <- similarity.matrix.dw %*% Z
  difs.dw <- exposure.observed.dw - (Z * CoppockJEPS$exposure.expected.1.dw + (1 - Z) * CoppockJEPS$exposure.expected.0.dw)
  (difs.dw - mean(difs.dw)) / sd(difs.dw)
}

# likewise, here's the test statistic
test.stat <- function(y, z) {
  e <- exposure(z)
  sum(resid(lm(y ~ z + e))^2)
}

set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    # the proper inference algorithm is a loop over each
    # parameter combination (directs, indirects):
    # 1. adjust the outcome using the observed Z and the params
    # 2. compute the test statistic using the observed Z and adjusted data
    # 3. repeatedly re-draw Z, and compute the test statistic using the adjusted data and the new Zs
    # 4. compare the observed test stat value to the reshuffled values

    direct.sim <- directs[j]
    indirect.sim <- indirects[k]
    
    pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
    pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim

    t.obs <- test.stat(pure.Y0, Z.obs)

    ssr.sims <- rep(NA,sims)
    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]
      ssr.sims[i] <- test.stat(pure.Y0, Z.sim)
    }
    
    pmat.ssr[j,k] <- mean(ssr.sims >= t.obs) # probability of seeing a statistic greater than observed
    
  }
}

direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)

graph.frame <- expand.grid(x=directs, y=indirects)
graph.frame$z <- as.vector(pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
fig2 <- levelplot(z~x*y, graph.frame, cuts=20, col.regions=col.l,
                  colorkey=FALSE,
                  at=depth.breaks,
                        ylab = "Hypothesized indirect effect",
                        xlab = "Hypothesized direct effect",
                        scales=list(x=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1)),
                                    y=list(at=round(seq(-.7, .2, by=.1), digits=1), labels=round(seq(-.7, .2, by=.1), digits=1))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(y0=-.5, y1= -.5, x0=directs[direct_breaks[1]], x1=directs[direct_breaks[2]], angle=90,code=3)
                          larrows(x0=-.6, x1= -.6, y0=indirects[indirect_breaks[1]], y1=indirects[indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)


pdf("CoppockJEPS_figure2.pdf")
print(fig2)
dev.off()

indirect.maxpvalue.2 <- median(indirects[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,2]])
direct.maxpvalue.2 <- median(directs[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,1]])

indirects.95.2 <- indirects[indirect_breaks]
directs.95.2 <- directs[direct_breaks]

pmat.ssr.2 <- pmat.ssr
save(pmat.ssr.2, indirect.maxpvalue.2, direct.maxpvalue.2, indirects.95.2, directs.95.2, file="fig2.rdata")



## Total Causal Effect Calculation
direct.sim <- direct.maxpvalue.2
indirect.sim <- indirect.maxpvalue.2
pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim
## Under Uniformity Trial
sum(pure.Y0, na.rm=TRUE)

## Observed
sum(Y.obs, na.rm=TRUE)

