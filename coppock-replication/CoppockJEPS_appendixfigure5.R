#### Coppock Replication code for JEPS --Hetero
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

all.fit.obs <-lm(Y.obs ~ Z.obs + exposure.obs + lowsupport + Z.obs*lowsupport+ exposure.obs*lowsupport, data=frame.obs)
ssr.obs <- sum(residuals(all.fit.obs)^2)

exposure.expected.1 <- CoppockJEPS$exposure.expected.1.dw
exposure.expected.0 <- CoppockJEPS$exposure.expected.0.dw

directs <-seq(from=-0.8, to=0.4, by=.1)
indirects <-seq(from=-0.8, to=0.4, by=.1)

sims <- 50
pmat.ssr <- array(NA, dim=c(length(directs), length(indirects), length(directs), length(indirects)))

set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    for(l in 1:length(directs)){
      for(m in 1:length(indirects)){
        
    direct.low.sim <- directs[j]
    indirect.low.sim <- indirects[k]
    direct.high.sim <- directs[l]
    indirect.high.sim <- indirects[m]
    

    ssr.sims <- rep(NA,sims)
    for(i in 1:sims){
      Z.sim <- Z_block[,1]
      exposure.sim <- (similarity.matrix.dw %*% Z.sim)
      exposure.sim.corrected <- exposure.sim - (Z.sim * exposure.expected.1 + (1-Z.sim)*exposure.expected.0)
      s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
      
      pure.Y0 <- rep(NA, 70)
      pure.Y0[lowsupport==1] <- Y.obs[lowsupport==1] + (-1*exposure.obs[lowsupport==1]*indirect.low.sim)  
      pure.Y0[lowsupport==0] <- Y.obs[lowsupport==0] + (-1*exposure.obs[lowsupport==0]*indirect.high.sim) 
      pure.Y0[Z.obs==1 & lowsupport==1] <- pure.Y0[Z.obs==1 & lowsupport==1] - direct.low.sim
      pure.Y0[Z.obs==1 & lowsupport==0] <- pure.Y0[Z.obs==1 & lowsupport==0] - direct.high.sim
      
      Y.sim <- rep(NA, 70)
      Y.sim[lowsupport] <-pure.Y0[lowsupport] + direct.low.sim*Z.sim[lowsupport] + indirect.low.sim*s.exposure.sim[lowsupport]
      Y.sim[!lowsupport] <-pure.Y0[!lowsupport] + direct.high.sim*Z.sim[!lowsupport] + indirect.high.sim*s.exposure.sim[!lowsupport]
      frame.sim <- data.frame(Y.sim, Z.sim, s.exposure.sim, lowsupport)
      Y.sim[lowsupport==0]
      Y.sim[lowsupport==1]
      
      all.fit.sim <-lm(Y.sim ~ Z.sim + s.exposure.sim + lowsupport + Z.sim*lowsupport+ s.exposure.sim*lowsupport, data=frame.sim)
      ssr.sims[i] <-sum(residuals(all.fit.sim)^2)
     
    }  
    pmat.ssr[j,k,l,m] <- mean(ssr.obs > ssr.sims)  
     }
    }
  }
}

pmat.ssr.hetero <- pmat.ssr
save(pmat.ssr.hetero, file="pmat.ssr.hetero")


peaks <- which(pmat.ssr == max(pmat.ssr,na.rm=TRUE), arr.ind = TRUE)

low.direct <- median(directs[peaks[,1]])
low.indirect <- median(indirects[peaks[,2]])
high.direct <- median(directs[peaks[,3]])
high.indirect <- median(indirects[peaks[,4]])
low.direct
low.indirect
high.direct
high.indirect

low.pmat.ssr <- pmat.ssr[,,7,6] ## Berger and Boos
high.pmat.ssr <- pmat.ssr[5,8,,] ## Berger and Boos


low.direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
low.indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)

high.direct_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=1, FUN=max) >.05)
high.indirect_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=2, FUN=max) >.05)



graph.frame <- expand.grid(x=directs, y=indirects)
graph.frame$z.low <- as.vector(low.pmat.ssr)
graph.frame$z.high <- as.vector(high.pmat.ssr)
col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
figA5a <- levelplot(z.low~x*y, graph.frame, cuts=20, col.regions=col.l,
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
                          larrows(y0=.2, y1= .2, x0=directs[low.direct_breaks[1]], x1=directs[low.direct_breaks[2]], angle=90,code=3)
                          larrows(x0=-.1, x1= -.1, y0=indirects[low.indirect_breaks[1]], y1=indirects[low.indirect_breaks[2]], angle=90,code=3)
                        },
                  legend = 
                    list(right = 
                           list(fun = draw.colorkey,
                                args = list(key = list(col = col.l, at = depth.breaks),
                                            draw = FALSE))),
)


figA5b <- levelplot(z.high~x*y, graph.frame, cuts=20, col.regions=col.l,
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



pdf("Replication Archive/figures/CoppockJEPS_appendixfigure5a.pdf")
print(figA5a)
dev.off()
pdf("Replication Archive/figures/CoppockJEPS_appendixfigure5b.pdf")
print(figA5b)
dev.off()

