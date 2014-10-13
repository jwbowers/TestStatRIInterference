#.libPaths("../.libraries")
##masterdir<-getwd()
if(length(grep("TestStatRIInterference$",getwd()))==0){setwd("..")}
require(compiler)
enableJIT(3)

library(RItools,lib.loc=".libraries")
options("RItools-lapply"=lapply)
options("RItools-sapply"=sapply)

source("simulation/setup.R")
source("code/teststatistics.R")

## for each of REPETITIONS draws from the randomization distribution, do simsamples tests.

sampler <- simpleRandomSampler(total = n, treated = n/2)
Zs <- sampler(REPETITIONS)$samples # discard the weight element

# the basic data has S, the network, and y0, the unif trial data used in the other sims
uniformityData <- simulationData(n,e)
##num1hopPeers<-colSums(uniformityData$S)
themodel <- interference.model.maker(uniformityData$S)
ssrTestStat<-SSRTmaker(uniformityData$S)
ssrNetTestStatistic<-ssrNetTestStatisticMaker(uniformityData$S)
ksNetResidTestStat<-ksNetResidTestStatisticMaker(uniformityData$S)
ksNetTestStatistic<-ksNetTestStatisticMaker(uniformityData$S)


dotestMaker<-function(model,y0,truth,TZ,thegrid,simsamples){
	force(model);force(y0);force(truth);force(TZ);force(thegrid);force(simsamples)
	function(z){
		y <- invertModel(model, y0, z, truth$beta, truth$tau)
		rit<-RItest(y,z, TZ, model, thegrid,samples=simsamples)
		return(cbind(rit@params, p = rit[-1, "p.value"])) ## inefficient but avoids errors later
		##return(rit[-1, "p.value"]) ## efficient but depends on RItools not changing
	}
}


## dotest<-dotestMaker(model=themodel,
## 		    y0=uniformityData$data$y0,
## 		    truth=TRUTH,
## 		    TZ=ssrTestStat,
## 		    thegrid=SEARCH,
## 		    simsamples=simsamples)

source("code/setup-clusters.R")
##require(snow)
##library(parallel)
##thecluster<-rep("localhost",8) ##rep(c("localhost","jwbowers.pol.illinois.edu"),c(8,8))
##thecluster<-rep(c("localhost","jwbowers.pol.illinois.edu"),c(12,8))
##cl<-makeCluster(thecluster,type="PSOCK")

## system.time(tmp<-dotest(Zs[,1]))
## Rprof(line.profiling=TRUE)
##tmp<-dotest(Zs[,1])
## Rprof(NULL)
## blah<-summaryRprof(lines="both")

##clusterEvalQ(cl,setwd("/Users/jwbowers/Documents/PROJECTS/FisherSUTVA/"))
clusterEvalQ(cl,.libPaths(".libraries"))
clusterEvalQ(cl,library(RItools)) ##,lib.loc=".libraries"))
clusterEvalQ(cl,library(compiler))
clusterEvalQ(cl,enableJIT(3))
clusterEvalQ(cl,source("code/teststatistics.R"))
clusterEvalQ(cl,options("RItools-lapply"=lapply))
clusterEvalQ(cl,options("RItools-sapply"=sapply))
## clusterExport(cl,"dotest")
clusterExport(cl,"growthCurve")


##clusterEvalQ(cl,if(length(grep("FisherSUTVA$",getwd()))==0){setwd("$HOME/Documents/PROJECTS/FisherSUTVA")})

testStats <- list("SSR Test Net Full" = ssrTestStat, ## test stat including a bit of the true model
		  "SSR Test Net Degree" = ssrNetTestStatistic, ## test stat including only network degree
		  "SSR Test" = ssrSimpleTestStatistic, ## test stat ignoring network
		  "KS Test Net Full" = ksNetResidTestStat, ## test stat including some of the true model
		  "KS Test Net Degree" = ksNetTestStatistic, ## test stat including only network degree
		  "KS Test" = ksTestStatistic  ## test stat ignoring network
		  )
##		  "Mann-Whitney U" = mann.whitney.u)
##		  "Mean Diff" = mean.difference,


## For debugging on the keeling cluster
##simsamples<-10
##Zs<-Zs[,1:10]
testStatResults<-vector("list",length=length(testStats))
names(testStatResults)<-names(testStats)
system.time(
for(i in 1:length(testStats)){
	TZ<-testStats[[i]]
	message(names(testStats)[i])
	dotest<-dotestMaker(model=themodel,
			    y0=uniformityData$data$y0,
			    truth=TRUTH,
			    TZ=TZ,
			    thegrid=SEARCH,
			    simsamples=simsamples)
	##dotestcmp<-cmpfun(dotest,options=list(optimize=3))
	clusterExport(cl,"dotest")
	clusterSetRNGStream(cl,iseed=rep(1,7)) ## use same stream of randomness for each test statistic
	testStatResults[[i]]<-parCapply(cl,Zs,function(z){ dotest(z)})
	##testStatResults[[i]]<-apply(Zs,2,function(z){ dotestcmp(z)})
}

save(testStatResults,file="simulation/teststat-parallel.rda")
stopCluster(cl)
)

##testStatTauPower <- simulationPower(testStatTauResults)

##testStatBetaResults <- lapply(testStats, function(TZ) {
##                              message(".",appendLF=FALSE)
##                              dotest(TZ,GRID=SEARCH.BETA,Zs=Zs)
##             })

##testStatBetaPower <- simulationPower(testStatBetaResults)

##save(file = "simulation/teststat.rda",
##     testStatTauResults, testStatTauPower,
##     testStatBetaResults, testStatBetaPower)

