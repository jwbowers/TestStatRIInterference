
## Functions to inspect the operating characteristics of our
## simulations and testStatistic

## Type I error rate: is the size of the test no greater than the
## level?

## ??Bias?? : Across randomizations is the mean of the point estimate
## equal to the true value.
require(RItools)

get.p.truth<-function(obj,true.params,alphas="auto"){
  
  theps<-p.values(obj)
  par.names<-names(obj@params)
  
  p.true.params<-theps[apply(theps[,par.names,drop=FALSE],1,function(x){
    identical(as.numeric(x),true.params)
  }),]

  return(p.true.params)
}

test.size<-function(sim.obj,truth,alphas="auto"){
  if(alphas[1]=="auto"){
    alphas<-sort(unique(unlist(lapply(sim.obj,function(obj){p.values(obj)$p}))))
  }
  p.truths<-sapply(sim.obj,get.p.truth,true.params=truth)
  
  res<-sapply(alphas,function(a){c(nominal.alpha=a,
                                   realized.alpha.rand=mean(p.truths<=a)
                                   )
                               })
  return(res)
}

  

get.2d.mn<-function(obj){ ##get 2d mean
  ##This function is useful for getting HL estimates (approx) with 2 params
  require(depth)
  pts<-na.omit(point(obj))
  par.nms<-grep("p",names(pts),invert=TRUE,value=TRUE)
  if(nrow(pts)!=1){
  mn<-trmean(pts[,par.nms],alpha=0)}else{
    mn<-as.numeric(pts[,par.nms])
  }
  
  return(mn)
}


get.2d.mn2<-function(obj){ ##get 2d mean of highest p-value hyps
  pts<-na.omit(obj)
  pts<-pts[pts[,"p"]==max(pts[,"p"]),,drop=FALSE]
  par.nms<-grep("p",names(pts),invert=TRUE,value=TRUE)
  if(length(par.nms)>1){
    if(nrow(pts)!=1){
      mn<-trmean(pts[,par.nms],alpha=0)
    }else{
      mn<-as.numeric(pts[,par.nms])
    }
  }else{
    mn<-mean(pts[,par.nms])
  }
  return(mn)
}

get.2d.mn3<-function(pts){
  require(depth)
  ##get 2d mean of highest p-value hyps from a piece of a list like
  ##sim4.points<-apply(sim4.arr,1,function(x){x[x[,"p"]==max(x[,"p"]),c("tau","tau2"),drop=FALSE]})
  par.nms<-colnames(pts)
  if(length(par.nms)>1){
    if(nrow(pts)!=1){
      mn<-trmean(pts[,par.nms],alpha=0)
    }else{
      mn<-as.numeric(pts[,par.nms])
    }
  }else{
    mn<-mean(pts[,par.nms])
  }
  return(mn)
}

get.2d.med<-function(pts){
  require(depth)
  ##get 2d median of highest p-value hyps from a piece of a list like
  ##sim4.points<-apply(sim4.arr,1,function(x){x[x[,"p"]==max(x[,"p"]),c("tau","tau2"),drop=FALSE]})
  par.nms<-colnames(pts)
  if(length(par.nms)>1){
    if(nrow(pts)!=1){
      md<-med(pts[,par.nms],method="Spatial")$median
    }else{
      md<-as.numeric(pts[,par.nms])
    }
  }else{
    md<-median(pts[,par.nms])
  }
  return(md)
}

get.p.truth2<-function(obj,true.params){
  ##true.params is a named list of parameters and true values
  colnames(obj)<-gsub("params.","",colnames(obj)) ## a hack: fix next time run the largeN simulations.
  par.names<-names(true.params) ##grep("params",colnames(obj),value=TRUE)
  goodrows<-apply(obj[,par.names,drop=FALSE],1,function(x){
    all.equal(as.numeric(x),as.numeric(true.params))
  })
  p.true.params<-obj[goodrows,]
  return(p.true.params$p)
}

get.p.truth.from.arrnm<-function(arrnm,thetrue.params=true.params){
  ##print(arrnm)
  is.2parm<-regexpr("1parm",arrnm)<0 ##returns -1 if not matched
  true.params<-thetrue.params[[is.2parm+1]]
  true.params<-true.params[true.params>0]
  thearr<-get(arrnm)
  ##params<-apply(thearr[,,c("tau1","tau2")],c(3),function(x){ length(unique(as.numeric(x))) })
  ##true.params<-sapply(names(as.list(params[params>1])),function(nm){ get(nm)},simplify=FALSE); print(true.params)
  get.p.truth3(thearr,true.params=true.params)
}

get.p.truth3<-function(thearr,true.params){ ##operates on arrays, vectorized
  logilist<-lapply(names(true.params),function(parmnm){ 
    simp.eq.test(thearr[,,parmnm],true.params[[parmnm]]) }) ##needs simp.eq.test because of floating point fun
  if(length(logilist)==1){
    thetest<-logilist[[1]]}else{
      thetest<-do.call("&",logilist  )
    }
  return(na.omit(thearr[,,"p"][thetest]))
}


test.size2<-function(sim.obj,truth,alphas,digits=4,tol=.Machine$double.eps ^ 0.5){
  ##This is the tolerance from all.equal() for class numeric
 if(alphas[1]=="auto"){
    alphas<-sort(unique(unlist(lapply(sim.obj,function(obj){obj$p}))))
  }
  p.truths<-sapply(sim.obj,get.p.truth2,true.params=truth)
  
  res<-sapply(alphas,function(a){c(nominal.alpha=a,
                                   realized.alpha.rand=mean(p.truths-a <= tol)
                                   )
                               })
  return(res)
}

test.size3<-function(sim.obj.arr,truth,
                     alphas=round(seq(.01,1,.01),1),
                     tol=.Machine$double.eps ^ 0.5){
  ##This is the tolerance from all.equal() for class numeric
 if(alphas[1]=="auto"){
    alphas<-sort(unique(as.vector(sim.obj[,,"p"])))
  }
 
 p.truths <- na.omit(sim1.arr[,,"p"][ simp.eq.test(sim1.arr[,,"tau"], tau) & 
                                     simp.eq.test(sim1.arr[,,"tau2"], tau2) ]
                     )
  
  res<-sapply(alphas,function(a){c(nominal.alpha=a,
                                   realized.alpha.rand=mean(p.truths - a <= tol)
                                   )
                               })
  return(res)
}


simp.eq.test<-function(x,y,tol=.Machine$double.eps ^ 0.5){
##A simple test for equality of two objects, in essence allowing us to use
  abs(x - y) < tol
}

sim.se<-function(nsim,p=.5){
  ## Est of standard error on p-values from simulations
  sqrt((p*(1-p))/nsim)
}

max.p.value.parms<-function(mat,parm.nms){
  x<-na.omit(mat)
  x[x[,"p"]==max(x[,"p"]),parm.nms,drop=FALSE]
}

make.power.mat<-function(arr,thealpha=(1/20),params=c("tau","tau2")){
  return(
         cbind(arr[1,,params],
               p=apply(arr[,,"p"],c(2),function(x){ 
                 mean(x<=thealpha) 
               })
               )
         )
}

