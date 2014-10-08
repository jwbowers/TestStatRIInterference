# These are some test statistics for permutation based randomization distributions.

require(RItools)

baseKSTest<-function(y,z){
	# next borrowed from KS test
	# next borrowed from KS test
	# first, set up using the KS test var names
	x <- y[z == 1]
	y <- y[z == 0]

	x <- x[!is.na(x)]
	n <- length(x)

	y <- y[!is.na(y)]
	n.x <- as.double(n)
	n.y <- length(y)

	n <- n.x * n.y/(n.x + n.y)
	w <- c(x, y)
	z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
	if (length(unique(w)) < (n.x + n.y)) {
		z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
	}

	return(max(abs(z)))
}

ksNetResidTestStatisticMaker <- function(S){
	## This function represents a bit of the true model of propagation in addition to knowledge about the network
	force(S)
	total1HopPeers<-colSums(S) ## S is undirected and symmetric
	function(y, z) {
		e<-lm.fit(cbind(1,z,as.vector(z %*% S),total1HopPeers),y)$residuals
		baseKSTest(y=e,z=z)
	}
}


ksNetTestStatisticMaker <- function(S){
	## This function represents a bit of the true model of propagation in addition to knowledge about the network
	force(S)
	total1HopPeers<-colSums(S) ## S is undirected and symmetric
	function(y, z) {
		e<-lm.fit(cbind(1,z,total1HopPeers),y)$residuals
		baseKSTest(y=e,z=z)
	}
}



ssrSimpleTestStatistic<- function(y,z){
	sum(lm.fit(cbind(1,z),y)$residuals^2) #resid(lm(y~z+d+total1HopPeers))^2)
}



ssrNetTestStatisticMaker<-function(S){
	## This statistic only takes into account the degree of the nodes.
	force(S)
	total1HopPeers<-colSums(S) ## degree, S is undirected and symmetric so colSums=rowSums
	function(y,z){
		sum(lm.fit(cbind(1,z,total1HopPeers),y)$residuals^2) #resid(lm(y~z+d+total1HopPeers))^2)
	}
}



