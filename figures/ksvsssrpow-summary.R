# A file to summarize the results of the KS versus SSR power comparison for no
# interference but difference models of effects and outcomes

if(length(grep("TestStatRIInterference$",getwd(),ignore.case=TRUE))==0){setwd("..")}
if(length(grep("TestStatRIInterference",.libPaths(),ignore.case=TRUE))==0){.libPaths(".libraries") }
options(width=132,digits=4)
source("simulation/setup.R")
load("simulation/ksvsssrpow-results.rda")
load("simulation/simplealts.rda")
load("simulation/simpletruth.rda")
load("simulation/simpledat.rda")

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


pdf(file="figures/ksvsssroutcomes.pdf",width=8,height=4)
par(mfrow=c(1,2),pty="s",mgp=c(1.5,.5,0),oma=rep(0,4),mar=c(3,3,2,0))
plot(density(simpledat$y0norm),main="Normal Outcome")
rug(simpledat$y0norm)
plot(density(simpledat$y0zif),main="Geometric Outcome")
rug(simpledat$y0zif)
dev.off()


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


source("code/plotting.R")

par(mfrow=c(1,2))
plotCompareModels(simpledat$y0norm,
		  Z=simpledat$Z.1,
		  make.data=function(y,Z){y},
		  models=list(COA=givenParams(constant.additive.model,tau=ALTS$COA$tau[60]),
			      COM=givenParams(constant.multiplicative.model,tau=ALTS$COM$tau[60])
			      )
		  )

plotCompareModels(jitter(simpledat$y0zif),
		  Z=simpledat$Z.1,
		  make.data=function(y,Z){y},
		  models=list(COA=givenParams(constant.additive.model,tau=ALTS$COA$tau[60]),
			      COM=givenParams(constant.multiplicative.model,tau=ALTS$COM$tau[60])
			      )
		  )



