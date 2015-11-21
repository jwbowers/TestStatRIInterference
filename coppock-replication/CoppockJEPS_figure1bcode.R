#### Coppock Replication code for JEPS -- FIGURE 1b
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

Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
low.support <- as.numeric(CoppockJEPS$full_co)
fit.obs <- lm(Y.obs ~ Z.obs + Z.obs*low.support +  low.support)

cate.lowsupport.0.obs <- fit.obs$coefficients[2]
cate.lowsupport.1.obs <- fit.obs$coefficients[2] + fit.obs$coefficients[4]

ssr.obs <- sum(residuals(fit.obs)^2)

cates.lowsupport.1 <-seq(from=-.8, to=.4, by=.025)
cates.lowsupport.0 <-seq(from=-.6, to=.5, by=.025)

sims <- 100
pmat.ssr <- matrix(NA, length(cates.lowsupport.0), length(cates.lowsupport.1))
set.seed(343)
for(j in 1:length(cates.lowsupport.0)){
  for(k in 1:length(cates.lowsupport.1)){
    cate.lowsupport.0.sim <- cates.lowsupport.0[j]
    cate.lowsupport.1.sim <- cates.lowsupport.1[k]
    
    ssr.sims <- rep(NA,sims)
    
    for(i in 1:sims){
      Z.sim <- Z_block[,i]
      
      pure.Y0 <- Y.obs
      pure.Y0[Z.obs==1 & low.support==0] <- pure.Y0[Z.obs==1 & low.support==0] - cate.lowsupport.0.sim
      pure.Y0[Z.obs==1 & low.support==1] <- pure.Y0[Z.obs==1 & low.support==1] - cate.lowsupport.1.sim
      
      Y.sim <- pure.Y0
      Y.sim[Z.sim==1 & low.support==0] <- Y.sim[Z.sim==1 & low.support==0] + cate.lowsupport.0.sim
      Y.sim[Z.sim==1 & low.support==1] <- Y.sim[Z.sim==1 & low.support==1] + cate.lowsupport.1.sim      
      
      fit.sim <- lm(Y.sim ~ Z.sim + Z.sim*low.support + low.support)
      ssr.sims[i] <- sum(residuals(fit.sim)^2)
    }
    
    pmat.ssr[j,k] <- mean(ssr.obs > ssr.sims)
    
  }
}

lowsupport0_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
lowsupport1_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)


graph.frame <- expand.grid(x=cates.lowsupport.0, y=cates.lowsupport.1)
graph.frame$z <- as.vector(pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
fig1b <- levelplot(z~x*y, graph.frame, cuts=20, col.regions=col.l,
                   colorkey=FALSE,
                   at=depth.breaks,
                        ylab = "Hypothesized effect in lower-support districts",
                        xlab = "Hypothesized effect in higher-support districts",
                        scales=list(x=list(at=seq(-1, .5, by=.2), labels=seq(-1, .5, by=.2)),
                                    y=list(at=seq(-1, .5, by=.2), labels=seq(-1, .5, by=.2))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(x0=-.35, x1= -.35, y0=cates.lowsupport.1[lowsupport1_breaks[1]], y1=cates.lowsupport.1[lowsupport1_breaks[2]], angle=90,code=3)
                          larrows(y0=.15, y1= .15, x0=cates.lowsupport.0[lowsupport0_breaks[1]], x1=cates.lowsupport.0[lowsupport0_breaks[2]], angle=90,code=3)
                        },
                   legend = 
                     list(right = 
                            list(fun = draw.colorkey,
                                 args = list(key = list(col = col.l, at = depth.breaks),
                                             draw = FALSE))),
)

pdf("Replication Archive/figures/CoppockJEPS_figure1b.pdf")
print(fig1b)
dev.off()

low.maxpvalue.1b <- cates.lowsupport.1[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[2]]
high.maxpvalue.1b <- cates.lowsupport.0[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[1]]

low.95.1b <- cates.lowsupport.1[lowsupport1_breaks]
high.95.1b <- cates.lowsupport.0[lowsupport0_breaks]

pmat.ssr.1b <- pmat.ssr
save(pmat.ssr.1b, high.95.1b,low.95.1b, high.maxpvalue.1b, low.maxpvalue.1b, file="Replication Archive/modeloutputs/fig1b.rdata")
  