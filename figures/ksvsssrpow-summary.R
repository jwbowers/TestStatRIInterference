# A file to summarize the results of the KS versus SSR power comparison for no interference but difference models of effects and outcomes

if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }
source("simulation/setup.R")
load("simulation/ksvsssrpow-results.rda")
ALTS<-list(tau=sort(unique(c(0,seq(-5,5,length=100)))))

apply(powComparison,2,summary)

t(powComparison[powComparison[,"resultsCOANormKS"]<1,])


