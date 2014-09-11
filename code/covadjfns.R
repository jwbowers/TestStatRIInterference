##Functions for covariance adjustment

### Used in interference.Rnw

interference.hypothesis.fn <- function(ys,z,tau0,w) {
  # a helper function to create constant additive models of effect
  ##for each tau0 return a list of functions adjusting outcomes in accord with the interference hypotheses
  ##assuming constant, additive treatment effects and constant spillover when spillover is hypothesized to occur
  fn1<-function(ys,z,tau0){ ys - (z * tau0) }
  ##adding (1-zi) into next just to ensure it only is picked up when zi is control
  fn2<-function(ys,zi,zj,tau0,w){ ys - (zi * tau0) - (w * tau0 * zj*(1-zi))  }
  ##rownnames(z)<-rownames(ys) ##make sure that treatment assignment has names
  targets<-c("Richland","Midland","Saginaw")
  sources<-c("Yakima","Saginaw","Midland");names(sources)<-targets
  ##print(tau0)
  adjys<-sapply(rownames(z),function(nm){ ##I bet we could use outer() to make this much faster
    ##print(tau0)
    if(nm %in% targets){
      newy<-fn2(ys[nm,],zi=z[nm,],zj=z[sources[nm],],tau0=tau0,w=w)
    }
    if(!(nm %in% targets)){
      newy<-fn1(ys[nm,],z[nm,],tau0=tau0)
    }
    return(newy)
  })
  names(adjys)<-rownames(z)
  return(adjys)
}



covadjI.fn<-function(thefmla,tau0,thedata,X,lam,R,w,z=thez,teststats=test.statistics.aligned){ ##I for interference
    ## X are covariates, Z is binary treatment, s is strata, R is outcome 
    ## lam is tuning parameter for lasso
    ## tau0 is a hypothesized effect (here entertaining constant treatment effect)
    ## thefmla is a covariance adjustment formula.
    ###stopifnot(length(R)==length(Z)) ## add other checks, be better if we could use names
  ## w is a weight (how much spillover)
  ##testsstats is a list of test statistic functions
    
  adjr<-interference.hypothesis.fn(ys=R,z=z,tau0=tau0,w=w)

    if(thefmla=="unadj"){
      thee<-adjr
      stopifnot(is.null(lam)) ##we shouldn't have a lambda if we are not fitting a model
    }else{
      themf<-model.frame(as.formula(thefmla),data=X) ## adjr is found in environment of function.
      
      ##make lambda behave correctly when we have an intercept
      themf.terms<-attr(themf,"terms")
      if(attr(themf.terms,"intercept")==1){ 
        if(length(lam)==1){
          lam<-c(0,rep(lam,length(attr(themf.terms,"term.labels")))) ##intercept is not penalized
        }
        if(length(lam)>1){
          lam<-c(0,lam)
        }
      } ## else lam is just passed through as is
      
      ##eventually allow a more general adjuster/residualizer
      thee<-resid(rq(as.formula(thefmla),data=themf,tau=.5,method="lasso",lambda=lam))
    }

    Om.ps<-sapply(names(teststats),function(nm){
      thefn<-teststats[[nm]]
      obs<-thefn(blocks=thedata$s,ys=thee,z=thedata$z)  ####Assumes thedata$s indicates blocks/strata
      dist<-sapply(Om,function(theZ){ thefn(blocks=thedata$s,ys=thee,z=theZ) })
      lp<-mean(dist<=obs)
      hp<-mean(dist>=obs)
      midlp<- mean(dist < obs) + mean(dist == obs)/2
      midhp<- mean(dist > obs) + mean(dist == obs)/2
      midtp<-2*min(midlp,midhp)
      tp<-2*min(lp,hp)
      return(c(lp=lp,hp=hp,tp=tp,midlp=midlp,midhp=midhp,midtp=midtp))
    })
    return(Om.ps)
  }

##


sums.strata<-function(ys,blocks,z){
  mean(sapply(split(data.frame(ys,z),blocks),function(dat){sum(dat$ys*dat$z)}))
}

mean.diff.strata<-function(ys,blocks,z){
  mean(
       mapply(FUN = function(ys, z) {
         mean(ys*z)-mean(ys*(1-z))
         ##             as.numeric(((z %*% ys)/sum(z)) - (((1 - z) %*% ys)/sum(1 - z)))
       },
              ys = split(ys, blocks),
              z = split(z, blocks))
       )
}

mean.rank.diff.strata<-function(ys,z,blocks){ ##average difference in ranks between control and treated obs.
  mean(
       mapply(FUN = function(ys, z) {
         R<-rank(ys)
         mean(R*z)-mean(R*(1-z))
         ##((R*z)/sum(z))-((R*(1-z))/sum(1-z))
       },
              ys = split(ys, blocks),
              z = split(z, blocks))
       )
}

###
show.cis.fn<-function(covadjI.arr.obj,alpha=(1/8),test.stat="themean"){
  fixmat<-function(mat){
    stopifnot(is.null(dim(mat)))
    dim(mat)<-c(1,length(mat))
    dimnames(mat)<-dimnames(covadjI.arr.obj)[1:2]
    return(mat)
  }
  tpmat<-covadjI.arr.obj[,,"tp",test.stat]
  lpmat<-covadjI.arr.obj[,,"lp",test.stat]
  hpmat<-covadjI.arr.obj[,,"hp",test.stat]
  
  if(is.null(dim(tpmat))){ tpmat<-fixmat(tpmat)}
  if(is.null(dim(lpmat))){ lpmat<-fixmat(lpmat)}
  if(is.null(dim(hpmat))){ hpmat<-fixmat(hpmat)}


  ci.widths<-sort(apply(tpmat,1,function(x){diff(range(as.numeric(names(x))[x>alpha]))}))
  ci.2sided<-apply(tpmat,1,function(x){range(as.numeric(names(x))[x>alpha])})
  ci.1sided.hi<-sort(apply(lpmat,1,function(x){max(as.numeric(names(x))[x>alpha])}))
  ci.1sided.low<-sort(apply(hpmat,1,function(x){min(as.numeric(names(x))[x>alpha])}))
  row.names(ci.2sided)<-c("ll","ul")

  cbind(width=ci.widths,
        two=t(ci.2sided[,names(ci.widths)]),
        hi=ci.1sided.hi[names(ci.widths)],
        lo=ci.1sided.low[names(ci.widths)])
}
