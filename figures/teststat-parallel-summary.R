
source("simulation/setup.R")
source("code/plotting.R")

load("simulation/teststat-parallel-results.rda")

searchParams<-expand.grid(SEARCH)

res<-cbind(data.frame(powResults),searchParams)

## Good Type I error for all tests
res[res$beta==TRUTH$beta & res$tau==TRUTH$tau,]

oneDplot<-function(x,f,...){
	## f is a vector of TRUE/FALSE selecting rows of res to plot
	## x is the name of the variable defining the x-axis
	plot(range(res[[x]]),c(0,1),type="n",ylab="Proportion p<.05",...)
	with(res[f,],{
		 lines(eval(parse(text=x)),ssrTestStat,col="red")
		 lines(eval(parse(text=x)),Mean.Diff,col="blue")
		 lines(eval(parse(text=x)),KS.Test,col="black")
		 lines(eval(parse(text=x)),Mann.Whitney.U,col="green") })
	ll<-TRUTH[[x]]-.1*diff(range(res[[x]]))
	ul<-TRUTH[[x]]+.1*diff(range(res[[x]]))
	segments(rep(ll,3),
			 .05+c(0,c(-2,2)*sim.se(REPETITIONS)),
			 rep(ul,3),
			 .05+c(0,c(-2,2)*sim.se(REPETITIONS)),col="gray",lwd=.5,lty=2)
	points(TRUTH[[x]],.05,pch=19,cex=2)
	legend("bottomright",
		   col=c("red","blue","black","green"),
		   legend=names(res)[1:4],
		   lwd=2,
		   bty="n")
}


## Power comparison holding tau fixed
par(mfrow=c(1,3),oma=rep(0,4),mgp=c(1.5,.5,0),pty="s")

oneDplot(x="beta",f=res$tau==TRUTH$tau,xlab="Beta")
oneDplot(x="beta",f=res$tau==min(SEARCH$tau),xlab="Beta")
oneDplot(x="beta",f=res$tau==max(SEARCH$tau),xlab="Beta")


## Power comparison holding beta fixed
par(mfrow=c(1,3),oma=rep(0,4),mgp=c(1.5,.5,0),pty="s")

oneDplot(x="tau",f=res$beta==TRUTH$beta,xlab="Tau")
oneDplot(x="tau",f=res$beta==min(SEARCH$beta),xlab="Tau")
oneDplot(x="tau",f=res$beta==max(SEARCH$beta),xlab="Tau")



## Two-D power
matlist<-lapply(res[,1:4],function(x){
				array(x,
			dim=sapply(SEARCH, length),
			dimnames=SEARCH)
		   })
names(matlist)<-names(res)[1:4]

pdf(file="figures/ssr2d.pdf")
plot2DPower(matlist[[1]],TRUTH,main=names(matlist)[1])
dev.off()

pdf(file="figures/meandiff2d.pdf")
plot2DPower(matlist[[2]],TRUTH,main=names(matlist)[2])
dev.off()

pdf(file="figures/kstest2d.pdf")
plot2DPower(matlist[[3]],TRUTH,main=names(matlist)[3])
dev.off()

pdf(file="figures/mannwhitneyu2d.pdf")
plot2DPower(matlist[[4]],TRUTH,main=names(matlist)[4])
dev.off()

system("touch figures/teststat2dfigs.txt")

