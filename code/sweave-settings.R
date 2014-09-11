options(SweaveHooks = list(
  # Sweave related options
  fig = function() {
         par(mar=c(3.5, 3, 1.1, 0),
         pty="s",
         mgp=c(1.5,0.5,0),
         oma=c(0,0,0,0))}),

  # echo = function() { 
  #         options(continue=" ") ##Don't show "+" prompts,
  #         options(prompt=" ")}),

  # error=function(){ ##show "+" prompts after errors
  #         options(prompt="> ",continue="+ ");NULL},

  # general options
  digits = 4,
  scipen = 5,
  show.signif.stars = FALSE)

set.seed(20110515)

# has the number of simulation samples been set in the evironment?
if (Sys.getenv("SAMPLES") == "") {
  SAMPLES <- 10000  
} else {
  SAMPLES <- as.numeric(Sys.getenv("SAMPLES")[[1]])  
}

# has the number of cores been set in the system evironment or in the running R workspace?
if (!exists("CORES")&Sys.getenv("CORES") == "") {
  CORES <- 2  ##default is 2 cores
}

if (!exists("CORES")&Sys.getenv("CORES") != "") {
  CORES <- as.numeric(Sys.getenv("CORES")[[1]])  
}

# a little helper to do sci. notation from
# http://stackoverflow.com/questions/8366324/r-sweave-formatting-numbers-with-sexpr-in-scientific-notation
sn <- function(x,digits)
{
  if (x==0) return("0")
  ord <- floor(log(abs(x),10))
  x <- x / 10^ord
  if (!missing(digits)) x <- format(x,digits=digits)
  if (ord==0) return(as.character(x))
  return(paste(x,"\\\\times 10^{",ord,"}",sep=""))
}

