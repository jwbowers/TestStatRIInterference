# A file to summarize the results of the KS versus SSR power comparison for no interference but difference models of effects and outcomes

if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }
source("simulation/setup.R")
load("simulation/ksvsssrpow-results.rda")
load("simulation/simplealts.rda")
load("simulation/simpletruth.rda")

apply(powCOA,2,summary)
apply(powCOM,2,summary)

t(powCOA[powCOA[,"resultsCOANormKS"]<1,])
t(powCOM[powCOM[,"resultsCOMNormKS"]<1,])

## SSR is slightly more power than KS for COA with Normal outcomes
## KS is more powerful than SSR for COA with Zif outcomes
## SSR is more powerful that KS for COM with Normal outcomes and less power than KS for Zif outcomes

pdf(file="KSVSSSRSimplePowplot.pdf",width=8,height=4)
par(mfrow=c(1,2),oma=rep(0,4),mgp=c(1.5,.5,0),pty="s",mar=c(3,3,2,0))
matplot(x=ALTS$COA$tau,powCOA,type="l",lty=c(1,2,1,2),col=c(1,1,2,2),
	main="Constant Additive Model",xlab="Hypotheses (Truth=0)",ylab="pr(p<=.05)")
legend(x="bottomleft",legend=substr(colnames(powCOA),11,50),lty=c(1,2,1,2),col=c(1,1,2,2),bty="n")
matplot(x=ALTS$COM$tau,powCOM,type="l",lty=c(1,2,1,2),col=c(1,1,2,2),
	main="Constant Multiplicative Model", xlab="Hypotheses (Truth=1)",ylab="pr(p<=.05)")
legend(x="bottomleft",legend=substr(colnames(powCOM),11,50),lty=c(1,2,1,2),col=c(1,1,2,2),bty="n")
dev.off()

