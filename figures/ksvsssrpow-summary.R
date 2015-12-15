# A file to summarize the results of the KS versus SSR power comparison for no interference but difference models of effects and outcomes

if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }
source("simulation/setup.R")
load("simulation/ksvsssrpow-results.rda")
load("simulation/simplealts.rda")
load("simulation/simpletruth.rda")

apply(powCOA,2,summary)
apply(powCOM,2,summary)

t(powCOA[powCOA[,"resultsCOANormKS"]<1,])
t(powCOM[powCOM[,"resultsCOMNormKS"]<1,])

## SSR is slightly more power than KS for COA with Normal outcomes
## KS is more powerful than SSR for COA with Zif outcomes
## SSR is more powerful that KS for COM with Normal outcomes and less power than KS for Zif outcomes
