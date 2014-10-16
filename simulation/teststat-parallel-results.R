## A file to extract power and error rate from the huge list of RItest objects

library(RItools,lib.loc=".libraries")

source("simulation/setup.R")

load("simulation/teststat-parallel.rda")
names(testStatResults)<- c("SSR Test Net Full" , ## test stat including a bit of the true model
		  "SSR Test Net Degree" , ## test stat including only network degree
		  "SSR Test" , ## test stat ignoring network
		  "KS Test Net Full" , ## test stat including some of the true model
		  "KS Test Net Degree" , ## test stat including only network degree
		  "KS Test" ## test stat ignoring network
		  )

library(abind)
tzResultsArrSSR<-abind(testStatResults[[1]],along=-1)


simPow2<-function(lst){
	m <- sapply(lst,function(l){ l$p})
	m <- m <= ALPHA
	rowMeans(m)
}

powResults<-lapply(testStatResults,function(obj){ simPow2(obj) })



##searchParams<-expand.grid(SEARCH)


##searchParams2<-testStatResults[[1]][[1]]@params
##stopifnot(all.equal(searchParams,searchParams2))

save(powResults,file="simulation/teststat-parallel-results.rda")



