## A file to extract power and error rate from the huge list of RItest objects

library(RItools,lib.loc=".libraries")

source("simulation/setup.R")

load("simulation/ksvsssrpowresults.rda")
ALTS<-list(tau=sort(unique(c(0,seq(-5,5,length=100)))))

simPow2<-function(lst){
	m <- sapply(lst,function(l){ l$p})
	m <- m <= ALPHA
	rowMeans(m)
}

powComparison<-sapply(ls(patt="resultsCO"),function(nm){
simPow2(get(nm))
})
stopifnot(length(unique(signif(ALTS$tau,4)))==length(ALTS$tau))
row.names(powComparison)<-signif(ALTS$tau,4)

apply(powComparison,2,summary)

powComparison[powComparison[,"resultsCOANormKS"]<1,]

save(powComparison,file="simulation/ksvsssrpow-results.rda")



