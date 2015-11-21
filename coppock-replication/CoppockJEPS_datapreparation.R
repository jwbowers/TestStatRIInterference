#### Public Opinion Replication
#### Butler, Daniel M., and David W. Nickerson. 2011. 
#### “Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment.” 
#### Quarterly Journal of Political Science 6(1): 55–83. DOI: 10.1561/100.00011019

rm(list=ls())
# Set your working directory
# setwd("")
library(foreign)  
library(maptools)
library(spdep)
library(wnominate)

#####################################################
# 1. Bring in Data from Butler and Nickerson (2012) #
# 2. Bring in Data from NM House Roll Calls in 2008 #
# 3. Replication Results from Butler and Nickerson  #
# 4. Create Exposure Model                          #
# 5. Randomization Inteference                      #
#####################################################

#####################################################
# 1. Bring in Data from Butler and Nickerson (2012) #
#####################################################
butler <- read.dta("nm.replication.dta")

butler <- within(butler,{
#Generate IDs
id <- 1:70
  
#Generate Presidential 2-party Vote Share
dem_vs <- kerry/(bush+kerry)

#Generate Governor Vote
v_richardson <- richardsongovdem/(richardsongovdem+dendahlgovrep)

#Generate dichotomous spending preferences.  full_co =1 if q2full is below the median. 
#(b/c the proposal was popular, the "treatment" should 
#have a larger effect among legislators 
#who learned that public support was low)

full_co <- q2full <= quantile(q2full, probs=.50)
lowsupport <- full_co
treatment_lowsupport <- treatment*lowsupport
treatment_highsupport <- treatment*(1-lowsupport)
})
#####################################################
# 2. Bring in 10,000 Block Random Assignments       #
#####################################################
load("CoppockJEPS_10000Randomizations.rdata")


#####################################################
# 3. Generate Similarity Scores                     #
#####################################################

nmhouse2008 <-read.csv("CoppockJEPS_rollcalldata.csv")
bills <- data.frame(nmhouse2008[5:21])

####Nominate Scores
bills_nona <- bills
bills_nona[bills_nona==99] <- NA
rollcalls <- rollcall(bills_nona)
nominate_scores <- wnominate(rollcalls, polarity=c(1, 2), minvotes=10)
dwnom_scores <- nominate_scores$legislators$coord1D

pdf("Replication Archive/figures/CoppockJEPS_appendixfigure1.pdf")
hist(dwnom_scores, breaks=50, main="", xlab="W-NOMINATE score")
dev.off()

get.similarity <- function(x, y){
  return((2-abs(x-y))/2)
}

similarity.matrix.dw <- matrix(NA, ncol=70, nrow=70)
for (i in 1:70){
  for (j in 1:70){
    similarity.matrix.dw[i,j] <- get.similarity(dwnom_scores[i], dwnom_scores[j])
  }
}
diag(similarity.matrix.dw) <- 0
similarity.matrix.dw[is.na(similarity.matrix.dw)==T] <- 0

exposure.block.dw <- similarity.matrix.dw %*% Z_block
pos.exposure.block.dw <- similarity.matrix.dw %*% (Z_block* (!butler$lowsupport))
neg.exposure.block.dw <- similarity.matrix.dw %*% (Z_block* (butler$lowsupport))

butler <- within(butler,{
  dwnom_scores <- dwnom_scores
  exposure.expected.1.dw <- rep(NA, 70)
  exposure.expected.0.dw <- rep(NA, 70)
  pos.exposure.expected.1.dw <- rep(NA, 70)
  pos.exposure.expected.0.dw <- rep(NA, 70)
  neg.exposure.expected.1.dw <- rep(NA, 70)
  neg.exposure.expected.0.dw <- rep(NA, 70)
  
  for (i in 1:70){
    exposure.expected.1.dw[i] <- mean(exposure.block.dw[i,Z_block[i,]==1])
    exposure.expected.0.dw[i] <- mean(exposure.block.dw[i,Z_block[i,]==0])
    pos.exposure.expected.1.dw[i] <- mean(pos.exposure.block.dw[i,Z_block[i,]==1])
    pos.exposure.expected.0.dw[i] <- mean(pos.exposure.block.dw[i,Z_block[i,]==0])
    neg.exposure.expected.1.dw[i] <- mean(neg.exposure.block.dw[i,Z_block[i,]==1])
    neg.exposure.expected.0.dw[i] <- mean(neg.exposure.block.dw[i,Z_block[i,]==0])
  }
  
  #Generate observed exposure variable
  exposure.observed.dw <- similarity.matrix.dw %*% treatment
  
  neg.exposure.observed.dw <- similarity.matrix.dw %*% (treatment*lowsupport)
  pos.exposure.observed.dw <- similarity.matrix.dw %*% (treatment*(!lowsupport))
  
  difs.dw <- exposure.observed.dw - (treatment * exposure.expected.1.dw + (1-treatment)*exposure.expected.0.dw)
  pos.difs.dw <- pos.exposure.observed.dw - (treatment * pos.exposure.expected.1.dw + (1-treatment)*pos.exposure.expected.0.dw)
  neg.difs.dw <- neg.exposure.observed.dw - (treatment * neg.exposure.expected.1.dw + (1-treatment)*neg.exposure.expected.0.dw)
  
  s.difs.dw <- (difs.dw - mean(difs.dw))/sd(difs.dw)
  s.pos.difs.dw <- (pos.difs.dw - mean(pos.difs.dw))/sd(pos.difs.dw)
  s.neg.difs.dw <- (neg.difs.dw - mean(neg.difs.dw))/sd(neg.difs.dw)
})


### Alternative parameterization of nominate distance using ranks

get.similarity.rank <- function(x, y){
  return((67-abs(x-y))/67)
}

dwnom_ranks <- rank(dwnom_scores, na.last="keep")

similarity.matrix.rank <- matrix(NA, ncol=70, nrow=70)
for (i in 1:70){
  for (j in 1:70){
    similarity.matrix.rank[i,j] <- get.similarity.rank(dwnom_ranks[i], dwnom_ranks[j])
  }
}
diag(similarity.matrix.rank) <- 0
similarity.matrix.rank[is.na(similarity.matrix.rank)==T] <- 0

exposure.block.rank <- similarity.matrix.rank %*% Z_block
pos.exposure.block.rank <- similarity.matrix.rank %*% (Z_block* (!butler$lowsupport))
neg.exposure.block.rank <- similarity.matrix.rank %*% (Z_block* (butler$lowsupport))

butler <- within(butler,{
  dwnom_scores <- dwnom_scores
  exposure.expected.1.rank <- rep(NA, 70)
  exposure.expected.0.rank <- rep(NA, 70)
  pos.exposure.expected.1.rank <- rep(NA, 70)
  pos.exposure.expected.0.rank <- rep(NA, 70)
  neg.exposure.expected.1.rank <- rep(NA, 70)
  neg.exposure.expected.0.rank <- rep(NA, 70)
  
  for (i in 1:70){
    exposure.expected.1.rank[i] <- mean(exposure.block.rank[i,Z_block[i,]==1])
    exposure.expected.0.rank[i] <- mean(exposure.block.rank[i,Z_block[i,]==0])
    pos.exposure.expected.1.rank[i] <- mean(pos.exposure.block.rank[i,Z_block[i,]==1])
    pos.exposure.expected.0.rank[i] <- mean(pos.exposure.block.rank[i,Z_block[i,]==0])
    neg.exposure.expected.1.rank[i] <- mean(neg.exposure.block.rank[i,Z_block[i,]==1])
    neg.exposure.expected.0.rank[i] <- mean(neg.exposure.block.rank[i,Z_block[i,]==0])
  }
  
  #Generate observed exposure variable
  exposure.observed.rank <- similarity.matrix.rank %*% treatment
  
  neg.exposure.observed.rank <- similarity.matrix.rank %*% (treatment*lowsupport)
  pos.exposure.observed.rank <- similarity.matrix.rank %*% (treatment*(!lowsupport))
  
  difs.rank <- exposure.observed.rank - (treatment * exposure.expected.1.rank + (1-treatment)*exposure.expected.0.rank)
  pos.difs.rank <- pos.exposure.observed.rank - (treatment * pos.exposure.expected.1.rank + (1-treatment)*pos.exposure.expected.0.rank)
  neg.difs.rank <- neg.exposure.observed.rank - (treatment * neg.exposure.expected.1.rank + (1-treatment)*neg.exposure.expected.0.rank)
  
  s.difs.rank <- (difs.rank - mean(difs.rank))/sd(difs.rank)
  s.pos.difs.rank <- (pos.difs.rank - mean(pos.difs.rank))/sd(pos.difs.rank)
  s.neg.difs.rank <- (neg.difs.rank - mean(neg.difs.rank))/sd(neg.difs.rank)
})


### Alternative parameterization of nominate distance using squared distance

get.similarity.sq <- function(x, y){
  return((4 - (x-y)^2)/4)
}

similarity.matrix.sq <- matrix(NA, ncol=70, nrow=70)
for (i in 1:70){
  for (j in 1:70){
    similarity.matrix.sq[i,j] <- get.similarity.sq(dwnom_scores[i], dwnom_scores[j])
  }
}
diag(similarity.matrix.sq) <- 0
similarity.matrix.sq[is.na(similarity.matrix.sq)==T] <- 0

exposure.block.sq <- similarity.matrix.sq %*% Z_block
pos.exposure.block.sq <- similarity.matrix.sq %*% (Z_block* (!butler$lowsupport))
neg.exposure.block.sq <- similarity.matrix.sq %*% (Z_block* (butler$lowsupport))

butler <- within(butler,{
  dwnom_scores <- dwnom_scores
  exposure.expected.1.sq <- rep(NA, 70)
  exposure.expected.0.sq <- rep(NA, 70)
  pos.exposure.expected.1.sq <- rep(NA, 70)
  pos.exposure.expected.0.sq <- rep(NA, 70)
  neg.exposure.expected.1.sq <- rep(NA, 70)
  neg.exposure.expected.0.sq <- rep(NA, 70)
  
  for (i in 1:70){
    exposure.expected.1.sq[i] <- mean(exposure.block.sq[i,Z_block[i,]==1])
    exposure.expected.0.sq[i] <- mean(exposure.block.sq[i,Z_block[i,]==0])
    pos.exposure.expected.1.sq[i] <- mean(pos.exposure.block.sq[i,Z_block[i,]==1])
    pos.exposure.expected.0.sq[i] <- mean(pos.exposure.block.sq[i,Z_block[i,]==0])
    neg.exposure.expected.1.sq[i] <- mean(neg.exposure.block.sq[i,Z_block[i,]==1])
    neg.exposure.expected.0.sq[i] <- mean(neg.exposure.block.sq[i,Z_block[i,]==0])
  }
  
  #Generate observed exposure variable
  exposure.observed.sq <- similarity.matrix.sq %*% treatment
  
  neg.exposure.observed.sq <- similarity.matrix.sq %*% (treatment*lowsupport)
  pos.exposure.observed.sq <- similarity.matrix.sq %*% (treatment*(!lowsupport))
  
  difs.sq <- exposure.observed.sq - (treatment * exposure.expected.1.sq + (1-treatment)*exposure.expected.0.sq)
  pos.difs.sq <- pos.exposure.observed.sq - (treatment * pos.exposure.expected.1.sq + (1-treatment)*pos.exposure.expected.0.sq)
  neg.difs.sq <- neg.exposure.observed.sq - (treatment * neg.exposure.expected.1.sq + (1-treatment)*neg.exposure.expected.0.sq)
  
  s.difs.sq <- (difs.sq - mean(difs.sq))/sd(difs.sq)
  s.pos.difs.sq <- (pos.difs.sq - mean(pos.difs.sq))/sd(pos.difs.sq)
  s.neg.difs.sq <- (neg.difs.sq - mean(neg.difs.sq))/sd(neg.difs.sq)
})



### Geographical -- connected if CDs touch

### Bring in shapefile
NM_geo <-readShapeSpatial("CoppockJEPSshapefile/hd_court_ordered.shp")
### Convert to Adjacency Matrix
similarity.matrix.geo <- nb2mat(poly2nb(NM_geo), style="B")
rownames(similarity.matrix.geo) <- NULL

exposure.block.geo <- similarity.matrix.geo %*% Z_block
pos.exposure.block.geo <- similarity.matrix.geo %*% (Z_block* (!butler$lowsupport))
neg.exposure.block.geo <- similarity.matrix.geo %*% (Z_block* (butler$lowsupport))


butler <- within(butler, {
  exposure.expected.1.geo <- rep(NA, 70)
  exposure.expected.0.geo <- rep(NA, 70)
  pos.exposure.expected.1.geo <- rep(NA, 70)
  pos.exposure.expected.0.geo <- rep(NA, 70)
  neg.exposure.expected.1.geo <- rep(NA, 70)
  neg.exposure.expected.0.geo <- rep(NA, 70)
  
  for (i in 1:70){
    exposure.expected.1.geo[i] <- mean(exposure.block.geo[i,Z_block[i,]==1])
    exposure.expected.0.geo[i] <- mean(exposure.block.geo[i,Z_block[i,]==0])
    pos.exposure.expected.1.geo[i] <- mean(pos.exposure.block.geo[i,Z_block[i,]==1])
    pos.exposure.expected.0.geo[i] <- mean(pos.exposure.block.geo[i,Z_block[i,]==0])
    neg.exposure.expected.1.geo[i] <- mean(neg.exposure.block.geo[i,Z_block[i,]==1])
    neg.exposure.expected.0.geo[i] <- mean(neg.exposure.block.geo[i,Z_block[i,]==0])
  }
  
  #Generate observed exposure variable
  exposure.observed.geo <- similarity.matrix.geo %*% treatment
  
  neg.exposure.observed.geo <- similarity.matrix.geo %*% (treatment*lowsupport)
  pos.exposure.observed.geo <- similarity.matrix.geo %*% (treatment*(!lowsupport))
  
  difs.geo <- exposure.observed.geo - (treatment * exposure.expected.1.geo + (1-treatment)*exposure.expected.0.geo)
  pos.difs.geo <- pos.exposure.observed.geo - (treatment * pos.exposure.expected.1.geo + (1-treatment)*pos.exposure.expected.0.geo)
  neg.difs.geo <- neg.exposure.observed.geo - (treatment * neg.exposure.expected.1.geo + (1-treatment)*neg.exposure.expected.0.geo)
  
  s.difs.geo <- (difs.geo - mean(difs.geo))/sd(difs.geo)
  s.pos.difs.geo <- (pos.difs.geo - mean(pos.difs.geo))/sd(pos.difs.geo)
  s.neg.difs.geo <- (neg.difs.geo - mean(neg.difs.geo))/sd(neg.difs.geo)
})



CoppockJEPS <- butler


save(Z_block, similarity.matrix.geo, similarity.matrix.dw, similarity.matrix.rank, similarity.matrix.sq, CoppockJEPS, file="CoppockJEPS.rdata")



