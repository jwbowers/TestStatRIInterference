#!/bin/bash
 
#SBATCH --job-name=teststat-parallel
#SBATCH -n 48
#SBATCH --time=144:00:00
#SBATCH --mem-per-cpu=8048
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user=jwbowers@illinois.edu
#SBATCH -p e
#SBATCH --export=PATH,R_LIBS,CLUSTER,CORES,LD_LIBRARY_PATH

set InputDir=/data/keeling/a/jwbowers/Documents/PROJECTS/TestStatRIInterference
set RunDir=/data/keeling/a/jwbowers/Documents/PROJECTS/TestStatRIInterference

cd /data/keeling/a/jwbowers/Documents/PROJECTS/TestStatRIInterference


export CLUSTER="keeling"
export CORES=36
export R_LIBS=.libraries

mpirun -n 1 -x LD_LIBRARY_PATH -x R_LIBS -x CORES -x CLUSTER make simulation/teststat-parallel.rda
