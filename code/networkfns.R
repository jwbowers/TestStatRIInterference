randomize <- function(n = 100, k = floor(n/2)) {
  z <- vector("logical", n)
  z[sample.int(n, k)] <- T
  return(z)
}

network <- function(n = 100, p = 0.25){

  # Generate matrix values, sampling 0 or 1 with given probabilities
  matvals = sample (c(0,1), n*(n-1)/2, replace = TRUE, prob = c(1-p, p))

  # From the values above, generate a symmetric matrix 
    networkmat = matrix(rep(0, n*n), ncol = n)
    mv = 1
    for (i in 1:n){
      for (j in 1:n){
        if (i>j) {
          networkmat [i,j] = matvals[mv]
            networkmat [j,i] = matvals[mv]
            mv = mv + 1
        }
      }
    }
  return(networkmat)
}

covariates <- function(n = 100) {
  x1 <- rnorm(n, 0, 10)
  x2 <- rbeta(n, 7, 1)
  data.frame(x1, x2)
}

within.block.network <- function(B) {
  n <- length(B) 
  S <- outer(X =B, Y =B, `==`)
  S <- replace(S, 1:n^2, as.numeric(S))
  diag(S) <- 0
  return(S)
}

# if both n and average.edges are odd, the average degree will be close but
# not exactly the requested value
average.edge.network <- function(n = 100, average.edges = 5) {
  total.edges <- floor(n * average.edges / 2)
  total.possible <- (n * (n - 1)) / 2
  edge.numbers <- sample.int(total.possible, total.edges)
  edges <- numeric(total.possible)
  edges[edge.numbers] <- 1
  S <- matrix(0, nrow = n, ncol = n)
  S[upper.tri(S)] <- edges
  tmp <- t(S)
  S[lower.tri(S)] <- tmp[lower.tri(tmp)]
  return(S)
}

