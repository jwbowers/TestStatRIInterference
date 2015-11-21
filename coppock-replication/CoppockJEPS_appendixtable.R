#### Coppock Replication code for JEPS -- TABLES
#### Butler, Daniel M., and David W. Nickerson. 2011. 
#### “Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment.” 
#### Quarterly Journal of Political Science 6(1): 55–83. DOI: 10.1561/100.00011019

rm(list=ls())
# Set your working directory
# setwd("")
library(xtable)
load("fig1a.rdata")
load("fig1b.rdata")
load("fig2.rdata")
load("fig3.rdata")
load("appendixfig2.rdata")
load("appendixfig3.rdata")
load("appendixfig4.rdata")


make_ci_text <- function(x){
  return(paste0("[", paste(round(x,3),collapse=","), "]"))
}

directs <- c(maxpvalue.1a, direct.maxpvalue.2, direct.maxpvalue.A2, direct.maxpvalue.A3,direct.maxpvalue.A4)
directs.ci <- c(make_ci_text(ate.95ci.1a),make_ci_text(directs.95.2),make_ci_text(directs.95.A2), make_ci_text(directs.95.A3), make_ci_text(directs.95.A4))
indirects <- c(NA, indirect.maxpvalue.2, indirect.maxpvalue.A2, indirect.maxpvalue.A3,indirect.maxpvalue.A4)
indirects.ci <- c(NA, make_ci_text(indirects.95.2), make_ci_text(indirects.95.A2), make_ci_text(indirects.95.A3), make_ci_text(indirects.95.A4))
model <- c("No Spillover", "Ideology (Absolute Distance)", "Ideology (Rank Distance)", "Ideology (Squared Distance)", "Geographic Contiguity")
corresponding_figure <- c("1a", "2", "A2", "A3", "A4")

constanteffects <- rbind(directs,directs.ci,indirects,indirects.ci,corresponding_figure ) 

constant.rownames <- c("Direct Effect", NA, "Indirect Effect", NA, "Corresponding Figure")

constant.xtable <- xtable(data.frame(row=constant.rownames, data.frame(constanteffects)))

print.xtable(constant.xtable, table.placement="H",caption.placement="top", only.contents=TRUE, include.colnames=FALSE, include.rownames=FALSE,hline.after=c(),
             file="/Users/Alex/Documents/Dropbox/Columbia/Collaboration/Work with me/Information Spillovers/JEPSrr/appendixtable1.tex")




directs.high <- c(high.maxpvalue.1b, high.direct.maxpvalue)
directs.high.ci <- c(make_ci_text(high.95.1b), make_ci_text(high.directs.95))
indirects.high <- c(NA, high.indirect.maxpvalue)
indirects.high.ci <- c(NA, make_ci_text(high.indirects.95))
directs.low <- c(low.maxpvalue.1b, low.direct.maxpvalue)
directs.low.ci <- c(make_ci_text(low.95.1b), make_ci_text(low.directs.95))
indirects.low <- c(NA, low.indirect.maxpvalue)
indirects.low.ci <- c(NA, make_ci_text(low.indirects.95))
model.highlow <- c("No Spillover", "Ideology (Absolute Distance)")
corresponding_figure_highlow <- c("1b", "3")
heterogeneouseffects <- rbind(directs.high,directs.high.ci,
                              indirects.high, indirects.high.ci,
                              directs.low,directs.low.ci,
                              indirects.low,indirects.low.ci,
                              corresponding_figure_highlow)



hetero.rownames <- c("Direct Effect -- High Support", NA, "Indirect Effect -- High Support", NA,
                     "Direct Effect -- Low Support", NA, "Indirect Effect -- Low Support", NA,"Corresponding Figure")

hetero.xtable <- xtable(data.frame(row=hetero.rownames, data.frame(heterogeneouseffects)))

print.xtable(hetero.xtable, table.placement="H",caption.placement="top", only.contents=TRUE, include.colnames=FALSE, include.rownames=FALSE,hline.after=c(),
             file="/Users/Alex/Documents/Dropbox/Columbia/Collaboration/Work with me/Information Spillovers/JEPSrr/appendixtable2.tex")


