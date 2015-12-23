
if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }

library(RItools)
source("simulation/setup.R")
source("code/teststatistics.R")

sampler <- simpleRandomSampler(total = n, treated = n/2)
Zs <- sampler(REPETITIONS)$samples # N x nsims

## Make two outcomes in control that both have 0 mean and 1 sd but with different distributions
set.seed(20151130)
tmpzif<-rgeom(n,prob=.7)
y0zif<-jitter(as.vector(scale(tmpzif))+10)
y0norm<-rnorm(n,mean=10)

summary(y0zif)
summary(y0norm)

simpledat<-data.frame(y0zif=y0zif,y0norm=y0norm,Z=Zs)

save(simpledat,file="simulation/simpledat.rda")

## Two models:
### One model is like a model of mean shift
constant.additive.model

### One model focuses changes on the shape of the dist
constant.multiplicative.model <- UniformityModel( function(y, z, tau) {
						   if(tau==0){
						     return(rep(0,length(y)))
						   } else {
						     y / ( 1 + z * ( tau-1 ) )
						   }
}, function(y_0, z, tau) { y_0 * ( 1 + z * ( tau-1 ) )})

## Simulation settings
### Different parameter settings can mean the same thing in the two different models
TRUTHAdditive<-list(tau=0)
TRUTHMultiplicative<-list(tau=1)
stopifnot(all.equal(constant.additive.model(y0norm,Zs[,1],0),constant.multiplicative.model(y0norm,Zs[,1],1)))
stopifnot(all.equal(constant.additive.model(y0zif,Zs[,1],0),constant.multiplicative.model(y0zif,Zs[,1],1)))

## Try to make the hypotheses in the multiplicative model similar in magnitude
## to the hypotheses of the multiplicative model (we cannot make them all
## exactly the same across all values of y0, but we can make them as close as
## possible)

ALTSAdditive<-list(tau=sort(unique(c(0,1,seq(-2,2,length=100)))))

addvsmult<-function(x,tau){
  addres<-invertModel(constant.additive.model,y0norm,Zs[,1],tau=tau)
  mulres<-invertModel(constant.multiplicative.model,y0norm,Zs[,1],tau=x*tau)
  median(abs(mulres-addres))
}

res<-sapply(ALTSAdditive$tau,function(thetau){
	      ##				message(thetau)
	      optim(par=c(0),fn=addvsmult,tau=thetau,method="BFGS",control=list(maxit=500))$par
})

## Check the optimization at a known solution
stopifnot(res[ALTSAdditive$tau==0]==0)

ALTSMultiplicative<-list(tau=ALTSAdditive$tau*res)
ALTSMultiplicative$tau[ALTSAdditive$tau==0]<-1

TRUTH<-list("COA"=TRUTHAdditive,
	    "COM"=TRUTHMultiplicative)

ALTS<-list("COA"=ALTSAdditive,
	   "COM"=ALTSMultiplicative)

save(ALTS,file="simulation/simplealts.rda")
save(TRUTH,file="simulation/simpletruth.rda")

system("touch simulation/ksvsssr-setup.txt")
