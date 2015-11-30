## A file to do a quick demonstration of the differences in power between SSR and KS  simple experiment with no interference

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

## Two models:
### One model is like a model of mean shift
coamodel<-constant.additive.model
### One model focuses changes on the shape of the dist
constant.multiplicative.model <- UniformityModel(function(y, z, tau) { y / ( 1 + z * ( tau-1 ) )  }, 
                                           function(y_0, z, tau) { y_0 * ( 1 + z * ( tau-1 ) )})

## quantchangemodel <- UniformityModel(function(y, z, tau) { y - z * tau }, 
##                                            function(y_0, z, tau) { y_0 + z * tau})
## 




## Simulation settings
ALTS<-list(tau=sort(unique(c(0,seq(-5,5,length=100)))))
TRUTH<-list(tau=0)
simsamples<-1000

dotestMaker<-function(model,y0,truth,TZ,thegrid,simsamples){
	force(model);force(y0);force(truth);force(TZ);force(thegrid);force(simsamples)
	function(z){
		y <- invertModel(model, y0, z, truth$tau)
		rit<-RItest(y,z, TZ, model, thegrid,samples=simsamples)
		return(cbind(rit@params, p = rit[-1, "p.value"])) ## inefficient but avoids errors later
		##return(rit[-1, "p.value"]) ## efficient but depends on RItools not changing
	}
}



testStats <- list( "SSR Test" = ssrSimpleTestStatistic, 
		  "KS Test" = ksTestStatistic  )

outcomes<-list("Norm"=y0norm,"Zif"=y0zif)

themodels<-list("COA"=constant.additive.model,
		"COM"=constant.multiplicative.model)

dokstestNorm<-dotestMaker(model=coamodel,
			    y0=y0norm,
			    truth=TRUTH,
			    TZ=ksTestStatistic,
			    thegrid=ALTS,
			    simsamples=simsamples)

kstestNormY<-dokstestNorm(Zs[,1])

dokstestZif<-dotestMaker(model=coamodel,
			    y0=y0zif,
			    truth=TRUTH,
			    TZ=ksTestStatistic,
			    thegrid=ALTS,
			    simsamples=simsamples)

kstestZifY<-dokstestZif(Zs[,1])




##ksTestStatistic
##ssrSimpleTestStatistic,


