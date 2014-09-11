
.libPaths(".libraries")
library(Rmpi)
library(parallel)

if (Sys.getenv("CORES") == "") {
  numcores <- detectCores() ##as.numeric(system("sysctl hw.ncpu | awk '{print $2}'",intern=TRUE)) 
} else {
  numcores <- as.numeric(Sys.getenv("CORES")[[1]])  
}

if(!exists("clusterswitch")){ clusterswitch<-Sys.getenv("CLUSTER") }
stopifnot(clusterswitch %in% c("","none","ec2","dkh","taub","keeling"))

if (clusterswitch == "none") {
  ##thecluster<-rep("localhost",numcores)
  ##cl<-makeCluster(thecluster)
}
if (clusterswitch == "") {
  thecluster<-rep("localhost",numcores)
  cl<-makeCluster(thecluster)
}
if (clusterswitch == "ec2"){
  nodes <- system("grep node[0-9] /etc/hosts | cut -d ' ' -f 2",intern=TRUE)
  mastercores<-length(system("ssh master  cat /proc/cpuinfo | grep processor | cut -d ':' -f 2",intern=TRUE))
  nodecores<-length(system("ssh node001 cat /proc/cpuinfo | grep processor | cut -d ':' -f 2",intern=TRUE))
  thecluster <- c(rep("master",mastercores),rep(nodes,each=nodecores))
  cl<-makeCluster(thecluster)
}
if (clusterswitch == "dkh"){
  nodes<-c(rep("localhost",numcores),rep("jwbowers.pol.illinois.edu",8))
  thecluster <- nodes
  cl<-makeCluster(thecluster)
}
if (clusterswitch == "taub" & Sys.getenv("PBS_NODEFILE") != "") {
  thecluster <- system("cat $PBS_NODEFILE|wc -l",intern=TRUE)
  cl<-makeCluster(thecluster[1],type="MPI")
}
if (clusterswitch == "keeling") {
  cl<-makeCluster(numcores,type="MPI")
}


