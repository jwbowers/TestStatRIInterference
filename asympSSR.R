

n<-1000
set.seed(12345)
y0<-rnorm(n)
y1<-y0+2#+rnorm(n,sd=.5)
z<-sample(rep(c(0,1),n/2))
y<-y1*z + (1-z)*y0
lm1<-lm(y~z)
e1<-resid(lm1)
obsssr<-sum(e1^2)

## The resids themselves look normal
qqnorm(e1)
qqline(e1)

## What is the randomizatino distribution of the sums of squares? (Chi-square of what kind?)gwap

### First show the relationship between the chisquare and Normal
getnorm<-function(){
  x<-rnorm(n,mean=mean(e1),sd=sd(e1))
  sum(x^2)
}

resN<-replicate(10000,getnorm())
thechi<-var(e1)*rchisq(10000,df=lm1$df)

summary(resN)
summary(thechi)

plot(density(resN),xlim=range(c(resN,thechi)))
lines(density(thechi),col="blue")

qqplot(resN,thechi)
qqline(y=thechi)
abline(0,1)



## Now show it for the sum of squared residuals

X<-cbind(1,z)
H<-X %*% solve(t(X) %*% X) %*% t(X)
blah<-hatvalues(lm1)
all.equal(diag(H),blah,check.attributes=FALSE)

## the posterior for sigma
sig<-res[1,]/rchisq(n=10000,df=998)

getssr<-function(z,y1,y0){
  thez<-sample(z)
  they<-y1*thez + (1-thez)*y0
  thelm<-lm(they~thez)
  thee<-resid(thelm)
  #tmp<-thee-mean(thee)
  return( c(ssr=sum(thee^2),ve=var(thee)) )
}


getchis<-function(z,y1,y0){
  thez<-sample(z)
  they<-y1*thez + (1-thez)*y0
  lm1<-lm(they~thez)
  lm0<-lm(they~1)
  rss1<-sum(resid(lm1)^2)
  rss0<-sum(resid(lm0)^2)
  rss0-rss1

  thelm<-lm(they~thez)
  thee<-resid(thelm)
  #tmp<-thee-mean(thee)
  return( c(ssr=sum(thee^2),ve=var(thee)) )
}



thechi1<-rchisq(10000,df=lm1$df)

res<-replicate(10000,getssr(z=z,y1=y1,y0=y0))
apply(res,1,summary)

summary(thechi1)

summary(res)
summary(tmp)

qqplot(tmp,res)
qqline(tmp)

plot(density(tmp))
lines(density(res))

