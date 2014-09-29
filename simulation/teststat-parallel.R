#.libPaths("../.libraries")
masterdir<-getwd()

library(RItools,lib.loc=".libraries")
options("RItools-lapply"=lapply)
options("RItools-sapply"=sapply)

source("simulation/setup.R")

## for each of REPETITIONS draws from the randomization distribution, do simsamples tests.

sampler <- simpleRandomSampler(total = n, treated = n/2)
Zs <- sampler(REPETITIONS)$samples # discard the weight element

# the basic data has S, the network, and y0, the unif trial data used in the other sims
uniformityData <- simulationData(n,e)
##num1hopPeers<-colSums(uniformityData$S)
themodel <- interference.model.maker(uniformityData$S)
ssrTestStat<-SSRTmaker(uniformityData$S)

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
clusterEvalQ(cl,options("RItools-lapply"=lapply))
clusterEvalQ(cl,options("RItools-sapply"=sapply))
## clusterExport(cl,"dotest")
clusterExport(cl,"growthCurve")


##clusterEvalQ(cl,if(length(grep("FisherSUTVA$",getwd()))==0){setwd("$HOME/Documents/PROJECTS/FisherSUTVA")})
##res<
testStats <- list("ssrTestStat" = ssrTestStat,
		  "Mean Diff" = mean.difference,
		  "KS Test" = ksTestStatistic,
		  "Mann-Whitney U" = mann.whitney.u)


## dotestSSR<-dotestMaker(model=themodel,
##  		    y0=uniformityData$data$y0,
##  		    truth=TRUTH,
##  		    TZ=ssrTestStat,
##  		    thegrid=SEARCH,
##  		    simsamples=simsamples)
## clusterExport(cl,"dotestSSR")
## 
## ssrResults<-parCapply(cl,Zs,function(z){ dotestSSR(z) })
## 
## save(ssrResults,file="simulation/ssrResults.rda")
## 

##system.time(

## For debugging on the keeling cluster
##simsamples<-100
##Zs<-Zs[,1:48]
testStatResults<-vector("list",length=length(testStats)) 
names(testStatResults)<-names(testStats)
for(i in 1:length(testStats)){
	TZ<-testStats[[i]]
	dotest<-dotestMaker(model=themodel,
			    y0=uniformityData$data$y0,
			    truth=TRUTH,
			    TZ=TZ,
			    thegrid=SEARCH,
			    simsamples=simsamples)
	clusterExport(cl,"dotest")
	testStatResults[[i]]<-parCapply(cl,Zs,function(z){ dotest(z)})
}
##	    testStatResults <- lapply(testStats, function(TZ) {
##				      message(".",appendLF=FALSE)
##				      dotest<-dotestMaker(model=themodel,
##							  y0=uniformityData$data$y0,
##							  truth=TRUTH,
##							  TZ=TZ,
##							  thegrid=SEARCH,
##							  simsamples=simsamples)
##				      clusterExport(cl,"dotest")
##				      parCapply(cl,Zs,function(z){ dotest(as.vector(z))})
##
##})
##	    )
##
##
save(testStatResults,file="simulation/teststat-parallel.rda")
##
stopCluster(cl)

##testStatTauPower <- simulationPower(testStatTauResults)

##testStatBetaResults <- lapply(testStats, function(TZ) {
##                              message(".",appendLF=FALSE)
##                              dotest(TZ,GRID=SEARCH.BETA,Zs=Zs)
##             })

##testStatBetaPower <- simulationPower(testStatBetaResults)

##save(file = "simulation/teststat.rda",
##     testStatTauResults, testStatTauPower,
##     testStatBetaResults, testStatBetaPower)
