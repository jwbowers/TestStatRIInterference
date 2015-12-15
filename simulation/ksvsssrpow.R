## A file to do a quick demonstration of the differences in power between SSR and KS  simple experiment with no interference

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
y0zif<-as.vector(scale(tmpzif))+10
y0norm<-rnorm(n,mean=10)

summary(y0zif)
summary(y0norm)

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

simsamples<-1000


## Trying to speed things up but not passing tests
## library(Rcpp)
## library(RcppArmadillo)
## sourceCpp("code/fastTestStatistics.cpp")
## testK<-apply(Zs,2,function(z){
## 				 ksTestStatistic(y0zif,z)
## 										   })
## testFastK<-apply(Zs,2,function(z){
## 				 ksFastTestStatistic(y0zif,z)
## 										   })
## stopifnot(all.equal(testK,testFastK))

dotestMaker<-function(model,y0,truth,TZ,thegrid,simsamples){
  force(model);force(y0);force(truth);force(TZ);force(thegrid);force(simsamples)
  function(z){
    y <- invertModel(model, y0, z, truth$tau)
    rit<-RItest(y,z, TZ, model, thegrid,samples=simsamples)
    return(cbind(rit@params, p = rit[-1, "p.value"])) ## inefficient but avoids errors later
  }
}

testStats <- list( "SSR" = ssrSimpleTestStatistic,
		  "KS" = ksTestStatistic  )

outcomes<-list("Norm"=y0norm,"Zif"=y0zif)

themodels<-list("COA"=constant.additive.model,
		"COM"=constant.multiplicative.model)

source("code/setup-clusters.R")
clusterEvalQ(cl,.libPaths(".libraries"))
clusterEvalQ(cl,library(RItools)) ##,lib.loc=".libraries"))
clusterEvalQ(cl,library(compiler))
clusterEvalQ(cl,enableJIT(3))
clusterEvalQ(cl,source("code/teststatistics.R"))
clusterEvalQ(cl,options("RItools-lapply"=lapply))
clusterEvalQ(cl,options("RItools-sapply"=sapply))

##simsamples<-10
## results<-array(c(length(testStats),length(outcomes),length(themodels),length(ALTS)))
for(i in 1:length(testStats)){
  for(j in 1:length(outcomes)){
    for(k in 1:length(themodels)){
      message(names(themodels)[k], names(outcomes)[j],names(testStats)[i])
      dotest<-dotestMaker(model=themodels[[k]],
			  y0=outcomes[[j]],
			  truth=TRUTH[[names(themodels)[k]]],
			  TZ=testStats[[i]],
			  thegrid=ALTS[[names(themodels)[k]]],
			  simsamples=simsamples)
      clusterExport(cl,"dotest")
      clusterSetRNGStream(cl,iseed=rep(1,7))
      assign(paste("results",names(themodels)[k],
		   names(outcomes)[j],
		   names(testStats)[i],sep=""),
	     parCapply(cl,Zs,function(z){dotest(z)}))
    }
  }
}

save(list=ls(patt="resultsCO"),file="simulation/ksvsssrpowresults.rda")
stopCluster(cl)
