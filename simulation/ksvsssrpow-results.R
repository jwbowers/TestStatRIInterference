## A file to extract power and error rate from the huge list of RItest objects

if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }

library(RItools) ##,lib.loc=".libraries")

source("simulation/setup.R")

load("simulation/ksvsssrpow.rda")
load("simulation/simplealts.rda")
load("simulation/simpletruth.rda")


simPow2<-function(lst){
  m <- sapply(lst,function(l){ l$p})
  m <- m <= ALPHA
  rowMeans(m)
}

powComparison<-sapply(ls(patt="resultsCO"),function(nm){
			simPow2(get(nm))
})

powCOA<-powComparison[,grep("COA",colnames(powComparison))]
stopifnot(length(unique(signif(ALTS$COA$tau,4)))==length(ALTS$COA$tau))
row.names(powCOA)<-signif(ALTS$COA$tau,4)


powCOM<-powComparison[,grep("COM",colnames(powComparison))]
stopifnot(length(unique(signif(ALTS$COM$tau,5)))==length(ALTS$COM$tau))
row.names(powCOM)<-signif(ALTS$COM$tau,5)



apply(powCOA,2,summary)
apply(powCOM,2,summary)

powCOA[powCOA[,"resultsCOANormKS"]<1,]
powCOM[powCOM[,"resultsCOMNormKS"]<1,]

save(powCOA,powCOM,file="simulation/ksvsssrpow-results.rda")


