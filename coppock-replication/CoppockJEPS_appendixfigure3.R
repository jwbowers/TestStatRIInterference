#### Coppock Replication code for JEPS -- Appendix Figure 3
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

load("Replication Archive/CoppockJEPS.rdata")

## Pick exposure model -- dw using squared distance
Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
exposure.obs <- CoppockJEPS$s.difs.sq

fit.obs <- lm(Y.obs ~ Z.obs + exposure.obs)
ssr.obs <- sum(residuals(fit.obs)^2)
direct.obs <- fit.obs$coefficients[2]
indirect.obs <- fit.obs$coefficients[3]

exposure.expected.1 <- CoppockJEPS$exposure.expected.1.sq
exposure.expected.0 <- CoppockJEPS$exposure.expected.0.sq

directs <-seq(from=-.7, to=0.5, by=.025)
indirects <-seq(from=-.5, to=0.7, by=.025)

sims <- 500
pmat.ssr <- matrix(NA, length(directs), length(indirects))
set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    direct.sim <- directs[j]
    indirect.sim <- indirects[k]
    
    ssr.sims <- rep(NA,sims)
    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]
      exposure.sim <- (similarity.matrix.sq %*% Z.sim)
      exposure.sim.corrected <- exposure.sim - (Z.sim * exposure.expected.1 + (1-Z.sim)*exposure.expected.0)
      s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
      pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
      pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim
      Y.sim <- pure.Y0 + direct.sim*Z.sim + indirect.sim*s.exposure.sim
      fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim)
      ssr.sims[i] <- sum(residuals(fit.sim)^2)
    }
    
    pmat.ssr[j,k] <- mean(ssr.obs > ssr.sims)
    
  }
}

direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)

graph.frame <- expand.grid(x=directs, y=indirects)
graph.frame$z <- as.vector(pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
figA3 <- levelplot(z~x*y, graph.frame, cuts=20, col.regions=col.l,
                  colorkey=FALSE,
                  at=depth.breaks,
                        ylab = "Hypothesized indirect effect",
                        xlab = "Hypothesized direct effect",
                        scales=list(x=list(at=round(seq(-.7, .5, by=.1),digits=1), labels=round(seq(-.7, .5, by=.1),digits=1)),
                                    y=list(at=round(seq(-.5, .7, by=.1), digits=1), labels=round(seq(-.5, .7, by=.1), digits=1))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(y0=.6, y1= .6, x0=directs[direct_breaks[1]], x1=directs[direct_breaks[2]], angle=90,code=3)
                          larrows(x0=.35, x1= .35, y0=indirects[indirect_breaks[1]], y1=indirects[indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)


pdf("Replication Archive/figures/CoppockJEPS_appendixfigure3.pdf")
print(figA3)
dev.off()


indirect.maxpvalue.A3 <- indirects[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[2]]
direct.maxpvalue.A3 <- directs[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[1]]

indirects.95.A3 <- indirects[indirect_breaks]
directs.95.A3 <- directs[direct_breaks]

pmat.ssr.A3<- pmat.ssr

save(pmat.ssr.A3, indirect.maxpvalue.A3, direct.maxpvalue.A3, indirects.95.A3, directs.95.A3, file="Replication Archive/modeloutputs/appendixfig3.rdata")

