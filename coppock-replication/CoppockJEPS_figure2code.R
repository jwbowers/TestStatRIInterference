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
pmat.ks  <- matrix(NA, length(directs), length(indirects))

# write a function to compute exposure as a function of a random treatment variable Z
exposure <- function(Z) {
  exposure.observed.dw <- similarity.matrix.dw %*% Z
  difs.dw <- exposure.observed.dw - (Z * CoppockJEPS$exposure.expected.1.dw + (1 - Z) * CoppockJEPS$exposure.expected.0.dw)
  (difs.dw - mean(difs.dw)) / sd(difs.dw)
}

# likewise, here's the test statistic
ssr.test.stat <- function(y, z) {
  e <- exposure(z)
  sum(resid(lm(y ~ z + e))^2)
}

# we get the KS test statistic from the our implemention
source("../code/teststatistics.R")

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

    t.ssr.obs <- ssr.test.stat(pure.Y0, Z.obs)
    t.ks.obs  <- baseKSTest(pure.Y0, Z.obs)
    
    ssr.sims <- rep(NA, sims)
    ks.sims  <- rep(NA, sims)

    for(i in 1:sims){
      Z.sim <- Z_block[,sample(1:10000, 1)]
      ssr.sims[i] <- ssr.test.stat(pure.Y0, Z.sim)
      ks.sims[i]  <- baseKSTest(pure.Y0, Z.sim)
    }
    
    # the SSR statistic is *decreasing* in evidence against the null
    # the KS statistic is *increasing* in evidence against the null 
    pmat.ssr[j,k] <- mean(t.ssr.obs >= ssr.sims)
    pmat.ks[j,k]  <- mean(t.ks.obs <= ks.sims)
    
  }
}

direct_breaks <- find_breaks(apply(pmat.ssr, MARGIN=1, FUN=max) >.05)
indirect_breaks <- find_breaks(apply(pmat.ssr, MARGIN=2, FUN=max) >.05)

indirect.maxpvalue.2 <- median(indirects[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,2]])
direct.maxpvalue.2 <- median(directs[which(pmat.ssr == max(pmat.ssr), arr.ind = TRUE)[,1]])

indirects.95.2 <- indirects[indirect_breaks]
directs.95.2 <- directs[direct_breaks]


save(pmat.ssr,
     pmat.ks,directs,
     indirects,
     direct_breaks,
     indirect_breaks,
     file = "fig2.rdata") 

