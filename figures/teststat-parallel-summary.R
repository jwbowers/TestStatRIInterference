
source("simulation/setup.R")
source("code/plotting.R")

load("simulation/teststat-parallel-results.rda")

searchParams<-expand.grid(SEARCH)

res<-cbind(data.frame(powResults),searchParams)

## Good Type I error for all tests
res[res$beta==TRUTH$beta & res$tau==TRUTH$tau,]

teststats<-names(res)[1:(ncol(res)-2)]

oneDplot<-function(x,f,...){
	## f is a vector of TRUE/FALSE selecting rows of res to plot
	## x is the name of the variable defining the x-axis
	plot(range(res[[x]]),c(0,1),type="n",ylab="Proportion p<.05",...)
	with(res[f,],{
	     for(i in 1:length(teststats)){
		 lines(eval(parse(text=x)),eval(parse(text=teststats[i])),col=i)
	     }})
	ll<-TRUTH[[x]]-.1*diff(range(res[[x]]))
	ul<-TRUTH[[x]]+.1*diff(range(res[[x]]))
	segments(rep(ll,3),
			 .05+c(0,c(-2,2)*sim.se(REPETITIONS)),
			 rep(ul,3),
			 .05+c(0,c(-2,2)*sim.se(REPETITIONS)),col="gray",lwd=.5,lty=2)
	points(TRUTH[[x]],.05,pch=19,cex=2)
	legend("bottomright",
		   col=1:length(teststats),
		   legend=teststats,
		   lwd=2,
		   bty="n")
}


## Power comparison holding tau fixed
par(mfrow=c(1,2),oma=rep(0,4),mgp=c(1.5,.5,0),pty="s")

oneDplot(x="beta",f=res$tau==TRUTH$tau,xlab="Beta")
##oneDplot(x="beta",f=res$tau==min(SEARCH$tau),xlab="Beta")
##oneDplot(x="beta",f=res$tau==max(SEARCH$tau),xlab="Beta")


## Power comparison holding beta fixed
##par(mfrow=c(1,3),oma=rep(0,4),mgp=c(1.5,.5,0),pty="s")

oneDplot(x="tau",f=res$beta==TRUTH$beta,xlab="Tau")
##oneDplot(x="tau",f=res$beta==min(SEARCH$beta),xlab="Tau")
##oneDplot(x="tau",f=res$beta==max(SEARCH$beta),xlab="Tau")



## Two-D power
matlist<-lapply(res[,1:(ncol(res)-2)],function(x){
				array(x,
			dim=sapply(SEARCH, length),
			dimnames=SEARCH)
		   })
names(matlist)<-names(res)[1:(ncol(res)-2)]

thetitles<-c("SSR Test w/ Treated Neighbors and Degree",
	     "SSR Test w/ Degree",
	     "SSR Test w/o Network Information",
	     "KS Test w/ Treated Neighbors and Degree",
	     "KS Test w/ Degree",
	     "KS Test w/o Network Information")
names(thetitles)<-names(matlist)

for(i in 1:length(matlist)){
   pdf(file=paste("figures/",names(matlist)[i],".pdf",sep=""))
   par(mfrow=c(1,1),mgp=c(1.5,.5,0),oma=rep(0,4))
   plot2DPower(matlist[[i]],TRUTH,main=thetitles[names(matlist)[i]])
   dev.off()
}


pdf(file="figures/twoDplots.pdf",width=8,height=6)
par(mfrow=c(2,3),mgp=c(1.5,.5,0),oma=rep(0,4),pty="s",mar=c(3,3,1,.5))
for(i in 1:length(matlist)){
   plot2DPower(matlist[[i]],TRUTH,main=thetitles[names(matlist)[i]],cex.lab=1,cex.main=.9)
}
dev.off()

system("touch figures/teststat2dfigs.txt")



