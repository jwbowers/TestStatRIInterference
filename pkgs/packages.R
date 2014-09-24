pkgs <- c(
"abind",
"SparseM",
"xtable",
"lattice",
"colorspace",
"igraph") 

to.install <- Filter(x = pkgs, f = function(p) {
  return(!require(p, character.only = T))
})

# use the Iowa mirror:
options(repos = "http://streaming.stat.iastate.edu/CRAN")
if (length(to.install) > 0) {
  thepkgs<-download.packages(to.install, destdir = ".libraries")
system(paste("R CMD INSTALL -l .libraries", paste(thepkgs[,2],collapse=" ")))
}
