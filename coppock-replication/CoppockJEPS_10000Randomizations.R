#10,000 Random Assignments for B&N

rm(list=ls())
# Set your working directory
# setwd("")
 
library(foreign)
butler <- read.dta("nm.replication.dta")

### Random assignment using the matching categories defined by B&N

block.ra <- function(blockvar, m=NULL){
  if(!all(mean(table(blockvar))==table(blockvar))){stop("Blocks not equal size")}
  assign <- rep(NA, length(blockvar))
  blocks <- unique(blockvar)
  for(k in 1:length(blocks)){
    N.block <- length(assign[blockvar==blocks[k]])
    m.block <- floor(N.block/2)
    if(!is.null(m)){m.block<-m}
    assign[blockvar==blocks[k]] <- ifelse(1:N.block %in% sample(1:N.block,m.block),1,0)
  }
  return(assign)
}

set.seed(1234567)  
Z_block <- replicate(10000, block.ra(blockvar=butler$match_category))

save(Z_block,file="CoppockJEPS_10000randomizations.rdata")
