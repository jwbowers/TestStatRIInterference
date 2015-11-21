#### Coppock Replication code for JEPS -- FIGURE 1a
#### Butler, Daniel M., and David W. Nickerson. 2011. 
#### “Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment.” 
#### Quarterly Journal of Political Science 6(1): 55–83. DOI: 10.1561/100.00011019

rm(list=ls())
# Set your working directory
# setwd("")



library(lattice)

find_breaks <- function(x){
  breaks <- rep(NA, length(x)-1)
  for(i in 1:length(breaks)){
    breaks[i+1] <- x[i]!=x[i+1]
  }
  return(which(breaks))
}

load("Replication Archive/CoppockJEPS.rdata")

Z.obs <- CoppockJEPS$treatment
Y.obs <- CoppockJEPS$sb24
fit.obs <- lm(Y.obs ~ Z.obs)

ate.obs <- fit.obs$coefficients[2]
ssr.obs <- sum(residuals(fit.obs)^2)

ates <-seq(from=-.5, to=.2, by=.01)

sims <- 100
pmat.ssr <- rep(NA, length(ates))
set.seed(343)
for(j in 1:length(ates)){
    ate.sim <- ates[j]
    
    ssr.sims <- rep(NA,sims)
    
    for(i in 1:sims){
      Z.sim <- Z_block[,i]
      pure.Y0 <- Y.obs
      pure.Y0[Z.obs==1] <- pure.Y0[Z.obs==1] - ate.sim
      
      Y.sim <- pure.Y0
      Y.sim[Z.sim==1] <- Y.sim[Z.sim==1] + ate.sim
      
      fit.sim <- lm(Y.sim ~ Z.sim)
      ssr.sims[i] <- sum(residuals(fit.sim)^2)
    }
    pmat.ssr[j] <- mean(ssr.obs > ssr.sims)
}

ate_breaks <- ates[find_breaks(pmat.ssr >.05)]

graph.frame <- data.frame(ates, pmat.ssr)

depth.breaks <- do.breaks(c(0,1), 20)
col.l <- colorRampPalette(c('white', 'black'))(1000)
graph.frame$color <-level.colors(pmat.ssr, at = depth.breaks, col.regions = col.l)

fig1a <- xyplot(pmat.ssr ~ ates, data = graph.frame, 
       aspect = "iso", groups = color, cex = 2, col = "black",   
       panel = function(x, y, groups, ..., subscripts) {
         fill <- groups[subscripts]
         #panel.grid(h = -1, v = -1)
         panel.xyplot(x, y, pch = 21, fill = fill, ...)
         panel.abline(v = 0, lty=2)
         larrows(x0=ate_breaks[1], x1= ate_breaks[2], y0=.4, y1=.4, angle=90,code=3)
       },
       legend = 
         list(right = 
                list(fun = draw.colorkey,
                     args = list(key = list(col = col.l, at = depth.breaks),
                                 draw = FALSE))),
       xlab = "Hypothesized average treatment effect", ylab = "p-values")




pdf("Replication Archive/figures/CoppockJEPS_figure1a.pdf")
print(fig1a)
dev.off()

maxpvalue.1a <- ates[which.max(pmat.ssr)]
ate.95ci.1a <- ate_breaks
pmat.ssr.1a <- pmat.ssr
save(maxpvalue.1a, ate.95ci.1a, pmat.ssr.1a,file="Replication Archive/modeloutputs/fig1a.rdata")



