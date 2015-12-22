################################################################################
# Plotting/graphing Routines
################################################################################
par(oma=rep(0,4))

## library(igraph)
## library(colorspace)
## 
## plotexperiment <- function(s, z, mode = "undirected",
##                            vertex.shape = ifelse(z, "circle", "square"),
##                            layout = layout.fruchterman.reingold,
##                            vertex.size = 6,
##                            vertex.color = ifelse(z, "#252525", "#A0A0A0"),
##                            vertex.label = NA,
##                            edge.color = "black",
##                            ...) {
## 
##   tmp <- graph.adjacency(s, mode = mode)
## 
##   plot(tmp,
##        vertex.shape = vertex.shape,
##        layout = layout,
##        vertex.size = vertex.size,
##        vertex.color = vertex.color,
##        vertex.label = vertex.label,
##        edge.color = edge.color,
##        margin=0,
##        ...)
## }
## 
## 
## make.filled.contour.plot<-function(mat,main,levs=10,col=rev(terrain_hcl(levs)),
##                                    addTruth=TRUE,autolab=TRUE,
##                                    thelevels=NULL,
##                                    true.params, ##names and values of the parameters
##                                    thez="p",
##                                    ...){
## 
##   params<-names(true.params)
## 
##   mat.p<- matrix(mat[,thez],
##                  nrow=length(unique(mat[,params[1]])),
##                  ncol=length(unique(mat[,params[2]])),
##                  dimnames=list(sort(unique(mat[,params[1]])),
##                                sort(unique(mat[,params[2]]))
##                                )
##                  )
##   theps<-sort(unique(as.numeric(mat.p)))
##   p.line.levels<-quantile(theps,seq(0,1,length=levs) )
##   if(addTruth){
##     p.truth<-mat[simp.eq.test(mat[,params[1]],true.params[[1]])&simp.eq.test(mat[,params[2]],true.params[[2]]),thez]
##     ##main<-paste(main,"(",thealpha,"test level=",round(p.truth,3),")")
##   }
##   ##p.line.levels<-sort(unique(c(p.truth,p.line.levels)))
##   if(!is.null(thelevels)){
##     thelabels<-thelevels
##   }else{
##     if(autolab){
##       thelabels <- NULL
##       thelevels <- pretty(range(mat.p,finite=TRUE),levs)
##     } else {
##       thelabels <- round(sort(unique(c(0,p.line.levels))),3)
##       thelevels <- sort(unique(c(0,p.line.levels)))
##     }
##   }
## 
##   parm1.values<-sort(unique(mat[,params[1]]))
##   parm2.values<-sort(unique(mat[,params[2]]))
## 
##   if(length(theps)==1){col<-col[length(col)]}
## 
##   image(list(x=parm1.values,y=parm2.values,z=mat.p),
##         main=main,
##         ##main=expression ( paste(main, "(Type I Error Rate", == round(p.truth,3), " for ",  alpha==thealpha,")") ),
##         ##main= expression(bquote(paste(.(main), "(Type I Error Rate" ==.(round(p.truth,3)), " for ",  alpha==.(thealpha),")") ) ),
##         #expression(paste(main,"( Type I Error Rate=",round(p.truth,3), " for ", alpha = (1/20))),
##         col=col,...)
##   if(length(theps)>1){
##     contour(list(x=parm1.values,y=parm2.values,z=mat.p),
##             ##nlevels=levs,
##             levels=thelevels,
##             labels=thelabels,
##             col="gray",
##             method="flattest",
##             labcex=.7,
##             vfont=c("sans serif","bold"),
##             add=TRUE)
##   }
##   if(addTruth) {points(true.params[1],true.params[2],pch=19,cex=2)}
## 
##   ##clines<-contourLines(x=tau1.values,y=tau2.values,z=mat.p,levels=p.truth)
##   ##lines(clines[[1]],lwd=2)
## }
## 
plotmods.fn<-function(i,true.params,themain=NULL,addrarerejects=TRUE,...){
  ##plot lots of sim results matrices
  is.2parm<-regexpr("1parm",i)<0 ##returns -1 if not matched
  thetruth<-true.params[[is.2parm+1]]
  thetruth<-thetruth[thetruth>0]
  params<-names(thetruth)

  if(is.numeric(i)){
    theobjnm <- apropos(paste("sim",i,".power",sep="")) ## ls() only in local environment ls(patt=paste("sim",i,".m.",sep=""))
  } else {
    theobjnm <- i
  }

  if(is.null(themain)){ themain<-abbreviate(theobjnm) }
  theobj<-get(theobjnm)

  ##One dim or two dim?
  parmcheck<-length(thetruth) ##apply(theobj[,params],2,function(x){ length(unique(x)) })
  ##if(addTruth) { rare.rejects<-theobj[theobj[,"p"]<=thealpha,,drop=FALSE] }

  if(parmcheck==1){
    ##theparm<-names(parmcheck[parmcheck!=1])
    plot(theobj[,params],theobj[,"p"],type="b",main=themain,ylim=c(0,1),xlab=params,ylab="Power")
    abline(h=thealpha)
  } else {
    make.filled.contour.plot(theobj,main=themain,true.params=thetruth,xlab=params[1],ylab=params[2],...)
    if(addrarerejects) {
      points(rare.rejects[,params[1]],rare.rejects[,params[2]],cex=2)
      rare.rejects<-theobj[theobj[,"p"]<=thealpha,,drop=FALSE]
    }
  }
}


plotmodels<-function(right.makeR, right.moe, wrong.makeR, wrong.moe, tau1, tau2, Yc, S, Z, B,...){
  ##require(colorspace)
  ##lots of extra code in this function that does nothing but slow it down.

  ZS<-as.vector(Z %*% S)
  n <- length(ZS)

  fakeZS<-seq(min(ZS),max(ZS),length=n) ##should only be an integer but ok for plotting as real.
  fakeYc<-seq(min(Yc),max(Yc),length=n)

  makeR.right<-right.makeR(fakeYc,tau1,tau2,S,...)
  moe.right<-right.moe(S,...)

  makeR.wrong<-wrong.makeR(fakeYc,tau1,tau2,S,...)
  makeR.wrong.obs<-wrong.makeR(Yc,tau1,tau2,S,...)
  moe.wrong<-wrong.moe(S,...)

  makeR.right.obs<-right.makeR(Yc,tau1,tau2,S,...)
  testR<-makeR.right.obs(Z,...)
  testYc<-moe.right(testR,Z,B,tau1,tau2)
  stopifnot(all.equal(Yc,testYc))

  righteffects<-makeR.right(z=rep(1,n),ZS=fakeZS)-makeR.right(z=rep(0,n),ZS=fakeZS)
  wrongeffects<-makeR.wrong.obs(z=rep(1,n),ZS=fakeZS)-makeR.wrong.obs(z=rep(0,n),ZS=fakeZS)

  ##thecolors.right<-diverge_hcl(2,h=c(90,0))
  ##thecolors.wrong<-diverge_hcl(2,h=c(260,180))
  allcolors<-diverge_hcl(4)
  thecolors.right<-allcolors[1:2]
  thecolors.wrong<-allcolors[3:4]

  par(mfrow=c(1,2),xpd=FALSE,oma=rep(0,4))

  plot(range(ZS),range(Yc),type="n",
       xlab="Number Treated Network Connections",
       ylab=expression(r[paste(i,",Z==1")] - r[paste(i,",Z==0")]),
       ylim=range(c(0,min(fakeYc),max(testR),righteffects,wrongeffects)),
       xlim=c(min(ZS),max(ZS)))
  ##,
  ##     col=thecolors.right[Z+1],
  ##     pch=c(19,21)[Z+1])

  lines(fakeZS,righteffects,
        lwd=2, col=thecolors.right[1])
  #lines(fakeZS,makeR.right(z=rep(0,n),ZS=fakeZS),lwd=2, col=thecolors.right[2])

  lines(fakeZS,wrongeffects,
        lwd=2, col=thecolors.wrong[2])
  #lines(fakeZS,makeR.wrong(z=rep(0,n),ZS=fakeZS),lwd=2, col=thecolors.wrong[2])

  theylim<-range( c( makeR.right(z=rep(1,n),ZS=fakeZS) ,makeR.right(z=rep(0,n),ZS=fakeZS), min(Yc), max(testR) ))
  plot(Yc, testR, xlab="Uniformity Trial Outcome",ylab="Observed Outcome",
       ylim=theylim, ##range(c(min(Yc),max(testR))),
       xlim=range(c(min(Yc),max(Yc),range(fakeYc))),
       col=thecolors.right[(!Z)+1],pch=c(19,21)[Z+1])
  lines(fakeYc,makeR.right(z=rep(1,n),ZS=fakeZS),lwd=2, col=thecolors.right[1])
  lines(fakeYc,makeR.right(z=rep(0,n),ZS=fakeZS),lwd=2, col=thecolors.right[2])
  lines(fakeYc,makeR.wrong(z=rep(1,n),ZS=fakeZS),lwd=2, col=thecolors.wrong[2])
  lines(fakeYc,makeR.wrong(z=rep(0,n),ZS=fakeZS),lwd=2, col=thecolors.wrong[1])

}

# The following function presents plots of competing models
# This will be easier in the latest RItools because of the integrated model
# objects.

plotCompareModels <- function(uniformity, # a vector of outcomes from unif trial
                              Z, # a treatment vector of 1s and 0s
                              make.data, # function(unif) -> observed
                              models,  # a list of models (fns(obs, b, Z) -> unif) to compare. NOTE: no params!
                              ... )
{
  # quick and dirty error checking, should be improved with msgs
  n <- length(uniformity)
  stopifnot(n == length(Z))

  stopifnot(inherits(make.data, "function"))
  stopifnot(all(sapply(models, function(i) { inherits(i, "function")})))

  models <- c("Obs." = function(y, z, b) { y }, models) # append this to the list of models

  k <- length(models)

  data <- make.data(uniformity, Z)

  adjusted.data <- as.vector(sapply(models, function(m) {
                                    m(data, Z)
                               }))

  ### do the plotting!
  ### the "groups" are uniformity, model1-control, model1-treatment, ...
  nt <- sum(Z)
  nc <- n - nt

  modelNames <- names(models)
  missingNames <- which(modelNames == "")
  modelNames[missingNames] <- paste("M", missingNames - 1, sep = "")

  # this is an ordered factor
  groups <- ordered(c(rep("Uniformity", n), as.vector(sapply(modelNames, function(m) {
                                                             paste(m, c("-C", "-T")[Z + 1], sep = "")
}))), c("Uniformity", as.vector(sapply(modelNames, function(m) {
  paste(m, c("-C", "-T"), sep = "")
}))))

  data.grouped <-  data.frame(y = c(uniformity, adjusted.data), grp = groups)

  boxwidth = 0.25

  # TODO: In RItools version, the addtion of a strip should be an option
  # outline in the previous plot should be !strip
  stripchart(y ~ grp,
             data = data.grouped,
             vertical = T,
             method = "jitter",
             col = "#999999",
             #group.names = c("Unif.", rep(c("0", "1"), k)))
             group.names = NA,
             ylab = "Outcome",
             cex = 1/3, ...)

  boxplot(y ~ grp, data = data.grouped,
          names = c("Unif.", rep(c("0", "1"), k)),
          ylab = NULL,
          boxwex = 2 * boxwidth,
          lwd = 0.75,
          outline = F,
          add = T)

  # put the model name under the assignment labels
  mtext(modelNames, at = 0.5 + 2 * 1:k, side = 1, line = 1.75)
  mtext("Z=", at = 1.5, side = 1, line = 1, adj = 0)

  # split up the plot into sections
  abline(v = c(1.5, 1.5 + 2 * (1:(k - 1))), lty = 2, col = "#cccccc")

  # compute means, could also provide medians, various quantiles
  group.means <- tapply(data.grouped$y, data.grouped$grp, mean)

  pts <- c(1, 1 + 1:(2 * k))
  segments(c(pts - boxwidth, pts - boxwidth), group.means,
           c(pts + boxwidth, pts + boxwidth), group.means,
           lty = 2)

}

# comparing models for a specific data set.
# m1, m2 are models of effect: f(y, z, b, params ...) -> uniformity
# there are some defaults to generate constant additive data
plotModelDistance <- function(m1, parameters1, m2, parameters2,
                              n = 100,
                              Z = rep(c(0,1), n/2),
                              observed.data = rnorm(100) + 2 * Z,
                              blocks = NULL,
                              distance = function(a,b) { sqrt(sum((a - b)^2)) },
                              plotter = levelplot, # wireframe also works
                              ...) # pass other args to plotter
{

  param.space.1 <- expand.grid(parameters1)
  param.space.2 <- expand.grid(parameters2)

  adj.data.1 <- apply(param.space.1, 1, function(params) {
                      do.call(m1, c(list(observed.data, Z, blocks), params))
                              })

  adj.data.2 <- apply(param.space.2, 1, function(params) {
                      do.call(m2, c(list(observed.data, Z, blocks), params))
                              })
  ## Each row in adj.data are the adjusted outcomes for all units given a model (m1 or m2) and hypotheses (params).

  cartesian.product <- expand.grid(1:(dim(param.space.1)[1]),
                                   1:(dim(param.space.2)[1]))

  # rename the parameters in case both have a paramter of the same name
  names(parameters1) <- paste("m1", names(parameters1), sep = ".")
  names(parameters2) <- paste("m2", names(parameters2), sep = ".")

  D <- array(apply(cartesian.product, 1, function(pair) {
                   distance(adj.data.1[, pair[[1]]], adj.data.2[, pair[[2]]])
  }),
             dim = c(sapply(parameters1, length),
                     sapply(parameters2, length)),
             dimnames = c(parameters1, parameters2))

  # this is cool: there is an array method for levelplot. it gets the names
  # right except for 2-d arrays (i.e. matrices)
  tmp <- names(dimnames(D))

  if (is.matrix(D)) {
    res <- plotter(D, xlab = tmp[1], ylab = tmp[2], ...)
  } else {
    k <- length(tmp) - 2
    res <- plotter(D,
                   ## key = list(space = "left",
                   ##   text = list(tmp[3:length(tmp)]),
                   ##   rectangles = list(pch = 21,
                   ##     col = trellis.par.get("strip.background")$col[1:k])),
                   ...)
  }

  return(res)

}

# usage examples for above
# two.param.model <- function(y, z, b, t1, t2) { y - t1^t2 * z }
# dims2 <- plotModelDistance(m1 = constant.additive.model,
#                            parameters1 = list(tau = -3:3),
#                            m2 = constant.multiplicative.model,
#                            parameters2 = list(beta = c(1:10/10)))
#
# (dims3 <- plotModelDistance(m1 = constant.additive.model,
#                            parameters1 = list(tau = -3:3),
#                            m2 = two.param.model,
#                            parameters2 = list(t1 = 0:10,
#                                               t2 = c(0, 1:5/5))))
#
# (dims4 <- plotModelDistance(m1 = two.param.model,
#                            parameters1 = list(t1 = 0:10,
#                                               t2 = c(0, 1:5/5)),
#                            m2 = two.param.model,
#                            parameters2 = list(t1 = 0:10,
#                                               t2 = c(0, 1:5/5))))

# helper function used for error bars on plots
sim.se <- function(nsim, p = .5){
  ## Est of standard error on p-values from simulations
  sqrt((p*(1-p))/nsim)
}

yaxislabel <- expression(paste("Prop. Rejected at ",alpha,"=.05"))
simulationPowerPlot <- function(sim, param) {
  plot(NULL,
       xlab = c("beta" = expression(beta), "tau" = expression(tau))[param], ylab = yaxislabel,
       xlim = range(SEARCH[param]), ylim = c(0,1), main = "Power")

  abline(v = TRUTH[param], col = "#AAAAAA")

  for(i in 1:length(sim)) {
    tmp <- data.frame(SEARCH[param], sim[[i]])
    lines(tmp, lty = i)
  }

}

simulationSizePlot <- function(sim, param) {

  idx <- which(SEARCH[[param]] == TRUTH[[param]])
  pvs <- sapply(sim, function(x) {
                sapply(x, function(y) {
                       y[idx + 1, "p.value"] # first row is the sharp null
                     })
       })

  plot(NULL,
       xlab = expression(paste(alpha,"(level)")),
       ylab = "Prop. Rejected (size)", ##yaxislabel,
       xlim = c(0,1),
       ylim = c(0,1),
       main = "Size")

  greycolor <- "#AAAAAA"

  # 45deg line
  segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, col = greycolor)

  # error bars
  lines(c(0,1), c(0,1) - 2 * sim.se(REPETITIONS), col = greycolor, lwd = .5, lty = 2)
  lines(c(0,1), c(0,1) + 2 * sim.se(REPETITIONS), col = greycolor, lwd = .5, lty = 2)

  for(i in 1:length(sim)) {
    e <- ecdf(pvs[, i]) ## y values, prop <= x
    k <- knots(e) ## x values
    lines(k , I(e(k)), lty = i)
  }


}

simSummaryPlots <- function(betapower, taupower, tauresults) {

  old <- par(mar = c(4,4,2,1))

  par(mgp=c(1.5,.5,0),tcl=-.3,oma=rep(0,4),mar=c(3,3,2,0),pty="s")
  #layout(matrix(1:4, nrow = 2, ncol = 2))
  #layout(matrix(c(1,3,2,4),nrow=1),widths=c(.5,1,1,1),respect=FALSE)
  par(mfrow=c(1,3))

  ##plot.new()
  ##legend(x = "center", legend = names(betapower), lty = 1:length(betapower),cex = 0.8)

  # column order placement
  simulationSizePlot(tauresults, "tau")
  legend(x = "topleft", legend = names(betapower), lty = 1:length(betapower),cex = 0.8)
  simulationPowerPlot(betapower, "beta")
  simulationPowerPlot(taupower, "tau")

  par(old)
}

plot2DPower <- function(power, truth = NULL, search = SEARCH, xlab =
                        expression(beta), ylab = expression(tau),
                        xlim = range(x, finite = TRUE),
                        ylim = range(y, finite = TRUE),main="",...) {
  params <- names(truth)
  pa <- function() { axis(1); axis(2)
  if (!is.null(truth)) {
    abline(v = truth[[1]])
    abline(h = truth[[2]])
  }
  }

  plot.new()
  y = as.numeric(search[[2]])
  x = as.numeric(search[[1]])
  z = power

  plot.window(xlim, ylim, "", xaxs = "i", yaxs = "i", asp = NA)
  .filled.contour( y=y,
                  x = x,
                  z = z,
                  col = grey.colors(20, 0, 1),
                  levels = seq(0, 1, by = 0.05))

  title(main = main, xlab = xlab, ylab = ylab,...)
  Axis(x, side = 1)
  Axis(y, side = 2)
  box()

  pa()

}
