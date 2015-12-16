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

load("CoppockJEPS.rdata")
source("../code/teststatistics.R")


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

directs <-seq(from=-5, to=5, length.out = 50)
indirects <-seq(from=-5, to=5, length.out = 50)

sims <- 50
low.pmat.ssr <- matrix(NA, length(directs), length(indirects))
high.pmat.ssr <- matrix(NA, length(directs), length(indirects))
ks.low.pmat <- matrix(NA, length(directs), length(indirects))

set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    direct.sim <- directs[j]
    indirect.sim <- indirects[k]
    
    low.ssr.sims <- rep(NA,sims)
    high.ssr.sims <- rep(NA,sims)
    ks.low.sims <- rep(NA, sims)
    
    pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
    pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim

    ks.obs <- baseKSTest(pure.Y0, Z.obs)
    
    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]
      exposure.sim <- (similarity.matrix.dw %*% Z.sim)
      exposure.sim.corrected <- exposure.sim - (Z.sim * exposure.expected.1 + (1-Z.sim)*exposure.expected.0)
      s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
      Y.sim <- pure.Y0 + direct.sim*Z.sim + indirect.sim*s.exposure.sim
      frame.sim <- data.frame(Y.sim, Z.sim, s.exposure.sim, lowsupport)
      low.fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim, data=subset(frame.sim, lowsupport==1))
      high.fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim, data=subset(frame.sim, lowsupport==0))
      low.ssr.sims[i] <- sum(residuals(low.fit.sim)^2)
      high.ssr.sims[i] <- sum(residuals(high.fit.sim)^2)

      ks.low.sims[i] <- baseKSTest(pure.Y0[lowsupport == 1], Z.sim[lowsupport == 1])
    }
    
    low.pmat.ssr[j,k] <- mean(low.ssr.obs > low.ssr.sims)
    high.pmat.ssr[j,k] <- mean(high.ssr.obs > high.ssr.sims)
    ks.low.pmat[j, k] <- mean(ks.low.sims >= ks.obs)
    
  }
}

low.direct_breaks <- find_breaks(apply(low.pmat.ssr, MARGIN=1, FUN=max) >.05)
low.indirect_breaks <- find_breaks(apply(low.pmat.ssr, MARGIN=2, FUN=max) >.05)

high.direct_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=1, FUN=max) >.05)
high.indirect_breaks <- find_breaks(apply(high.pmat.ssr, MARGIN=2, FUN=max) >.05)


n <- prod(dim(low.pmat.ssr))

tmp <- expand.grid(x=directs, y=indirects)

graph.frame <- rbind(tmp, tmp)
graph.frame$z <- c(as.vector(low.pmat.ssr), as.vector(ks.low.pmat))
graph.frame$type <- c(rep("SSR", n), rep("KS", n))

col.l <- colorRampPalette(c('white', 'black'))(1000)
depth.breaks <- do.breaks(c(0,1), 20)
fig3a <- levelplot(z ~ x*y | type, graph.frame, cuts = 20, col.regions = col.l)

pdf("CoppockJEPS_figure3a.pdf")
print(fig3a)
dev.off()
