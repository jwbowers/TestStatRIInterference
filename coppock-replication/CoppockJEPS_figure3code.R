#### Coppock Replication code for JEPS -- FIGURE 3
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



## Pick exposure model -- main analysis uses dw nominate
Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
exposure.obs <- CoppockJEPS$s.difs.dw
lowsupport <- CoppockJEPS$lowsupport

frame.obs <- data.frame(Z.obs, Y.obs, exposure.obs, lowsupport)

low.fit.obs <- lm(Y.obs ~ Z.obs + exposure.obs, data=subset(frame.obs, lowsupport==1))
low.ssr.obs <- sum(residuals(low.fit.obs)^2)
low.direct.obs <- low.fit.obs$coefficients[2]
low.indirect.obs <- low.fit.obs$coefficients[3]

high.fit.obs <- lm(Y.obs ~ Z.obs + exposure.obs, data=subset(frame.obs, lowsupport==0))
high.ssr.obs <- sum(residuals(high.fit.obs)^2)
high.direct.obs <- high.fit.obs$coefficients[2]
high.indirect.obs <- high.fit.obs$coefficients[3]

exposure.expected.1 <- CoppockJEPS$exposure.expected.1.dw
exposure.expected.0 <- CoppockJEPS$exposure.expected.0.dw

directs <-seq(from=-0.9, to=0.2, by=.025)
indirects <-seq(from=-0.5, to=0.2, by=.025)

sims <- 500
low.pmat.ssr <- matrix(NA, length(directs), length(indirects))
high.pmat.ssr <- matrix(NA, length(directs), length(indirects))
set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    direct.sim <- directs[j]
    indirect.sim <- indirects[k]
    
    low.ssr.sims <- rep(NA,sims)
    high.ssr.sims <- rep(NA,sims)
    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]
      exposure.sim <- (similarity.matrix.dw %*% Z.sim)
      exposure.sim.corrected <- exposure.sim - (Z.sim * exposure.expected.1 + (1-Z.sim)*exposure.expected.0)
      s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
      pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
      pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim
      Y.sim <- pure.Y0 + direct.sim*Z.sim + indirect.sim*s.exposure.sim
      frame.sim <- data.frame(Y.sim, Z.sim, s.exposure.sim, lowsupport)
      low.fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim, data=subset(frame.sim, lowsupport==1))
      high.fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim, data=subset(frame.sim, lowsupport==0))
      low.ssr.sims[i] <- sum(residuals(low.fit.sim)^2)
      high.ssr.sims[i] <- sum(residuals(high.fit.sim)^2)
    }
    
    low.pmat.ssr[j,k] <- mean(low.ssr.obs > low.ssr.sims)
    high.pmat.ssr[j,k] <- mean(high.ssr.obs > high.ssr.sims)
    
  }
}

low.direct_breaks <- find_breaks(apply(low.pmat.ssr, MARGIN=1, FUN=max) >.05)
low.indirect_breaks <- find_breaks(apply(low.pmat.ssr, MARGIN=2, FUN=max) >.05)

high.direct_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=1, FUN=max) >.05)
high.indirect_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=2, FUN=max) >.05)



graph.frame <- expand.grid(x=directs, y=indirects)
graph.frame$z.low <- as.vector(low.pmat.ssr)
graph.frame$z.high <- as.vector(high.pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
fig3a <- levelplot(z.low~x*y, graph.frame, cuts=20, col.regions=col.l,
                  colorkey=FALSE,
                  at=depth.breaks,
                        ylab = "Hypothesized indirect effect",
                        xlab = "Hypothesized direct effect",
                        scales=list(x=list(at=seq(-.9, .5, by=.1), labels=seq(-.9, .5, by=.1)),
                                    y=list(at=seq(-.9, .5, by=.1), labels=seq(-.9, .5, by=.1))),
                        panel = function(...) {
                          panel.levelplot(...)
                          panel.abline(h = 0, lty=2)
                          panel.abline(v = 0, lty=2)
                          larrows(y0=.1, y1= .1, x0=directs[low.direct_breaks[1]], x1=directs[low.direct_breaks[2]], angle=90,code=3)
                          larrows(x0=-.1, x1= -.1, y0=indirects[low.indirect_breaks[1]], y1=indirects[low.indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)


fig3b <- levelplot(z.high~x*y, graph.frame, cuts=20, col.regions=col.l,
                     colorkey=FALSE,
                     at=depth.breaks,
                     ylab = "Hypothesized indirect effect",
                     xlab = "Hypothesized direct effect",
                     scales=list(x=list(at=seq(-.9, .5, by=.1), labels=seq(-.9, .5, by=.1)),
                                 y=list(at=seq(-.9, .5, by=.1), labels=seq(-.9, .5, by=.1))),
                     panel = function(...) {
                       panel.levelplot(...)
                       panel.abline(h = 0, lty=2)
                       panel.abline(v = 0, lty=2)
                       larrows(y0=-.1, y1= -.1, x0=directs[high.direct_breaks[1]], x1=directs[high.direct_breaks[2]], angle=90,code=3)
                       larrows(x0=-.5, x1= -.5, y0=indirects[high.indirect_breaks[1]], y1=indirects[high.indirect_breaks[2]], angle=90,code=3)
                     },
                     legend = 
                       list(right = 
                              list(fun = draw.colorkey,
                                   args = list(key = list(col = col.l, at = depth.breaks),
                                               draw = FALSE))),
)








pdf("Replication Archive/figures/CoppockJEPS_figure3a.pdf")
print(fig3a)
dev.off()
pdf("Replication Archive/figures/CoppockJEPS_figure3b.pdf")
print(fig3b)
dev.off()



low.indirect.maxpvalue <- indirects[which(low.pmat.ssr == max(low.pmat.ssr), arr.ind = TRUE)[2]]
low.direct.maxpvalue <- directs[which(low.pmat.ssr == max(low.pmat.ssr), arr.ind = TRUE)[1]]

low.indirects.95 <- indirects[low.indirect_breaks]
low.directs.95 <- directs[low.direct_breaks]

high.indirect.maxpvalue <- indirects[which(high.pmat.ssr == max(high.pmat.ssr), arr.ind = TRUE)[1,2]]
high.direct.maxpvalue <- directs[which(high.pmat.ssr == max(high.pmat.ssr), arr.ind = TRUE)[2,1]]

high.indirects.95 <- indirects[high.indirect_breaks]
high.directs.95 <- directs[high.direct_breaks]


save(low.pmat.ssr, low.indirect.maxpvalue, low.direct.maxpvalue, low.indirects.95, low.directs.95,
     high.pmat.ssr, high.indirect.maxpvalue, high.direct.maxpvalue, high.indirects.95, high.directs.95,
     file="Replication Archive/modeloutputs/fig3.rdata")
