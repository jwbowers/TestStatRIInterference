## A file to extract power and error rate from the huge list of RItest objects

library(RItools,lib.loc=".libraries")

source("simulation/setup.R")

load("simulation/teststat-parallel.rda")

simPow2<-function(lst){
		   m <- sapply(lst,function(l){ l[-1,"p.value"]})
           m <- m <= ALPHA
		   rowMeans(m)
}

powResults<-lapply(testStatResults,function(obj){ simPow2(obj) })

searchParams<-expand.grid(SEARCH)
searchParams2<-testStatResults[[1]][[1]]@params
stopifnot(all.equal(searchParams,searchParams2))

save(powResults,file="simulation/teststat-parallel-results.rda")



