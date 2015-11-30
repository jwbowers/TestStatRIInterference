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
source("../code/teststatistics.R")

## Pick exposure model -- main analysis uses dw nominate
Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
exposure.obs <- CoppockJEPS$s.difs.dw

fit.obs <- lm(Y.obs ~ Z.obs + exposure.obs)
ssr.obs <- sum(residuals(fit.obs)^2)
direct.obs <- fit.obs$coefficients[2]
indirect.obs <- fit.obs$coefficients[3]

exposure.expected.1 <- CoppockJEPS$exposure.expected.1.dw
exposure.expected.0 <- CoppockJEPS$exposure.expected.0.dw

exposure <- function(Z) {
  exposure.sim <- (similarity.matrix.dw %*% Z)
  exposure.sim.corrected <- exposure.sim - (Z * exposure.expected.1 + (1-Z)*exposure.expected.0)
  s.exposure.sim <- (exposure.sim.corrected - mean(exposure.sim.corrected))/sd(exposure.sim.corrected)
}

exposure.computed <- exposure(Z.obs)

t.ssr.comp <- sum(residuals(lm(Y.obs ~ Z.obs + exposure.computed))^2)

directs <-seq(from=-.7, to=0.2, by=.025)
indirects <-seq(from=-.7, to=0.2, by=.025)

sims <- 50
pmat.ssr <- matrix(NA, length(directs), length(indirects))
pmat.ks  <- matrix(NA, length(directs), length(indirects))

pmat.ssr.comp <- matrix(NA, length(directs), length(indirects))
pmat.ks.comp  <- matrix(NA, length(directs), length(indirects))
pmat.ssr.comp.y0 <- matrix(NA, length(directs), length(indirects))

set.seed(343)
for(j in 1:length(directs)){
  for(k in 1:length(indirects)){
    direct.sim <- directs[j]
    indirect.sim <- indirects[k]

    pure.Y0 <- Y.obs - Z.obs * direct.sim - exposure.obs * indirect.sim
    t.ks.obs <- baseKSTest(pure.Y0, Z.obs)

    Y0.comp <- Y.obs - Z.obs * direct.sim - exposure.computed * indirect.sim
    t.ks.comp  <- baseKSTest(Y0.comp, Z.obs)
    t.ssr.comp.y0 <- sum(residuals(lm(Y0.comp ~ Z.obs + exposure.computed))^2)

    ssr.sims <- rep(NA, sims)
    ks.sims  <- rep(NA, sims)
    ssr.comp.sims <- rep(NA, sims)
    ks.comp.sims  <- rep(NA, sims)
    ssr.comp.y0.sims <- rep(NA, sims)

    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]

      s.exposure.sim <- exposure(Z.sim)
      Y.sim <- pure.Y0 + direct.sim*Z.sim + indirect.sim*s.exposure.sim

      fit.sim <- lm(Y.sim ~ Z.sim + s.exposure.sim)
      ssr.sims[i] <- sum(residuals(fit.sim)^2)

      # replicate Alex's style SSR test to see what the impact of computing the observed exposure is.
      Y.sim <- Y0.comp + direct.sim*Z.sim + indirect.sim*s.exposure.sim
      ssr.comp.sims[i] <- sum(residuals(lm(Y.sim ~ Z.sim + s.exposure.sim))^2)

      ## KS tests
      # first, use Y0 based on "exposure.obs"
      ks.sims[i] <- baseKSTest(pure.Y0, Z.sim)
      # second, use Y0 based on the computed version of observed exposure
      ks.comp.sims[i]  <- baseKSTest(Y0.comp, Z.sim)

      # finally, compute the SSR using Y0.comp
      ssr.comp.y0.sims[i] <- sum(residuals(lm(Y0.comp ~ Z.sim + s.exposure.sim))^2)
    }
    
    pmat.ssr[j,k] <- mean(ssr.obs > ssr.sims)
    pmat.ks[j, k] <- mean(ks.sims >= t.ks.obs)

    pmat.ssr.comp[j,k] <- mean(ssr.comp.sims <= t.ssr.comp)
    pmat.ks.comp[j,k]  <- mean(ks.comp.sims >= t.ks.comp)

    pmat.ssr.comp.y0[j,k] <- mean(ssr.comp.y0.sims <= t.ssr.comp.y0)
  }
}

direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)

indirect.maxpvalue.2 <- median(indirects[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,2]])
direct.maxpvalue.2 <- median(directs[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,1]])

indirects.95.2 <- indirects[indirect_breaks]
directs.95.2 <- directs[direct_breaks]

save(pmat.ssr,
     pmat.ks,
     pmat.ssr.comp,
     pmat.ks.comp,
     pmat.ssr.comp.y0,
     directs,
     indirects,
     direct_breaks,
     indirect_breaks,
     indirect.maxpvalue.2,
     direct.maxpvalue.2,
     indirects.95.2,
     directs.95.2,
     file="fig2.rdata")



## Total Causal Effect Calculation
direct.sim <- direct.maxpvalue.2
indirect.sim <- indirect.maxpvalue.2
pure.Y0 <- Y.obs + (-1*exposure.obs*indirect.sim)
pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - direct.sim
## Under Uniformity Trial
sum(pure.Y0, na.rm=TRUE)

## Observed
sum(Y.obs, na.rm=TRUE)

