## A file to do a quick demonstration of the differences in power between SSR and KS for simple constant effects models, simple experiment with no interference

if(length(grep("TestStatRIInterference$",getwd()))==0){setwd("..")}

library(RItools)
source("simulation/setup.R")
source("code/teststatistics.R")

sampler <- simpleRandomSampler(total = n, treated = n/2)
Zs <- sampler(REPETITIONS)$samples # N x nsims

## Make two outcomes in control that both have 0 mean and 1 sd but with different distributions
set.seed(20151130)
tmpzif<-rgeom(n,prob=.7)
y0zif<-scale(tmpzif)
y0norm<-rnorm(n)

summary(y0zif)
summary(y0norm)

## Truth is no effects
y1norm<-y0norm
y1zif<-y0zif

##Alternatives
ALTS<-seq(-10,10,length=100)
simsamples<-10
coamodel<-constant.additive.model

rit<-RItest(y,z, ksTestStatistic, coamodel, ALTS,samples=simsamples)
return(cbind(rit@params, p = rit[-1, "p.value"])) ## inefficient but avoids errors later

##ksTestStatistic
##ssrSimpleTestStatistic,


