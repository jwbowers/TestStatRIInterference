#!/bin/bash
 
#SBATCH --job-name=teststat-parallel
#SBATCH -n 36
#SBATCH --time=72:00:00
#SBATCH --mem-per-cpu=8048
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user=jwbowers@illinois.edu
#SBATCH -p e
#SBATCH --export=PATH,R_LIBS,CLUSTER,CORES

set InputDir=/data/keeling/a/jwbowers/Documents/PROJECTS/TestStatRIInterference
set RunDir=/data/keeling/a/jwbowers/Documents/PROJECTS/TestStatRIInterference

cd $HOME/Documents/PROJECTS/TestStatRIInterference

export CLUSTER="keeling"
export CORES=36
export R_LIBS=.libraries

mpirun -n 1 -x R_LIBS -x CORES -x CLUSTER make simulation/teststat-parallel.rda
