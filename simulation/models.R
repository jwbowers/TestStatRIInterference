## The UniformityModel is a datatype defined in the R package on CRAN. 
## The first argument is a function to go from observed data to the uniformity trial 
## The second argument is the inverse of the first model (it generates
## observed data from the uniformity trial)

## y0 is the uniformity trial
## y is the observed outcome
## z is 0 or 1 denoting treatment assignment

## Since this model depends on S, we need to generate for a specific S
## The growthCurve function specifies how spillover happens.

growthCurve <- function(beta, tau, x) {
  (beta + (1 - beta) * exp(-(tau^2) * x))
}

interference.model.maker <- function(S) {
  ## just to be safe about lazy argument evaluation,
  ## probably not needed
  force(S)
  
  ## we will return a UniformityModel object  
  UniformityModel(
    function(y, z, beta, tau) {
      zS <- as.vector(z %*% S)
      z * (1 / beta) * y +
        (1 - z) * y / growthCurve(beta, tau, zS)
    },
    function(y0, z, beta, tau) {
      zS <- as.vector(z %*% S)
      z * beta * y0 +
        (1 - z) * growthCurve(beta, tau, zS) * y0  
    })
}
