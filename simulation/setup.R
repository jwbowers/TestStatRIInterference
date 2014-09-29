require(RItools, lib.loc = ".libraries")

set.seed(20120614)

source("simulation/models.R")
# values we will use for creating data most often
TRUTH <- list(beta = 2, tau = 0.5)

set.seed(20120618)

MAXI <- 32
MAXN <- MAXI^2
ORDER <- sample.int(MAXN, MAXN)
UNIFORMITY <- runif(MAXN, min = 30, max = 70)
X1 <- sample.int(5, MAXN, replace = T)

# blocks: determines sample size
simulationData <- function(n, edges, makey0 = function(base, S) {
  base * growthCurve(TRUTH$beta, TRUTH$tau, rowSums(S))}) {

  # make sure both are integers
  edges <- floor(edges)
  n <- floor(n)

  stopifnot(n <= MAXN)
  ##stopifnot(edges <= choose(n, 2))
  if(edges > choose(n,2)){ warning("You may have an overly dense network.") }
  sampler <- simpleRandomSampler(total = n, treated = n / 2)


  nodes <- ORDER[1:n]
  cx <- nodes %% MAXI
  cy <- floor(nodes / MAXI)

  data <- data.frame( cx, cy)
  rownames(data) <- nodes

  dists <- dist(data[,c("cx", "cy")])

  tmp <- matrix(Inf, ncol = n, nrow = n)
  tmp[lower.tri(tmp, diag = F)] <- dists
  torder <- order(tmp)[1:edges]

  S <- matrix(0, ncol = n, nrow = n)
  if(edges!=0) { S[torder] <- 1 }
  S[upper.tri(S, diag = F)] <- t(S)[upper.tri(S, diag = F)]

  data$base <- UNIFORMITY[1:n]
  data$y0 <-  with(data, makey0(base, S))

  return(list(n = n,
              sampler = sampler,
              data = data,
              S = S))
}

DENSITY <- 2

# the environemental variable SEARCH_POINTS controls how fine grained a search
# to perform. More points means a more fine grained search, but takes more
# processing power.

if (Sys.getenv("SEARCH_POINTS") == "") {
  SEARCH_POINTS <- 20
} else {
  SEARCH_POINTS <- as.numeric(Sys.getenv("SEARCH_POINTS")[[1]])
}

# a helper to be used in a moment
sortu <- function(...) { sort(unique(c(...))) }

beta.range <- seq(0.1, 5, length.out = SEARCH_POINTS)
tau.range <- seq(0, 1.25, length.out = SEARCH_POINTS)

# beta = 1 is no effects
# tau = 0 is no spillover
# always include these in the search.
SEARCH <-
  list(beta = sortu(1, TRUTH$beta, beta.range),
       tau = sortu(0, TRUTH$tau, tau.range))

SEARCH.TAU <- list(beta = TRUTH$beta, tau = SEARCH$tau)
SEARCH.BETA <- list(beta  = SEARCH$beta, tau = TRUTH$tau)

if (Sys.getenv("REPETITIONS") == "") {
  REPETITIONS <- 1000
} else {
  REPETITIONS <- as.numeric(Sys.getenv("REPETITIONS")[[1]])
}

ALPHA <- 0.05 # reject hypothesis if p-value is <= ALPHA

simulationPower <- function(x) {
  lapply(x, function(lst) {
    # lst is a list of RItest results, which are data.frames
    # make a big matrix out of them
    m <- sapply(lst, function(x) { x[-1, "p.value"] })
    m <- m <= ALPHA # see setup for alpha level
    rowMeans(m)
  })
}


# A simulation using different uniformity trials
n <- 256
e <- n * DENSITY
simsamples<-10000
##REPTITIONS<-100


