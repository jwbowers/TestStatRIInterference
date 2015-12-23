load("simulation/simpledat.rda")

pdf(file="figures/ksvsssr-outcomes.pdf",width=8,height=4)
par(mfrow=c(1,2),pty="s",mgp=c(1.5,.5,0),oma=rep(0,4),mar=c(3,3,2,0))
plot(density(simpledat$y0norm),main="Normal Outcome")
rug(simpledat$y0norm)
plot(density(simpledat$y0zif),main="Geometric Outcome")
rug(simpledat$y0zif)
dev.off()


