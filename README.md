TestStatRandInfInterference
===========================

Test statistic selection for randomization inference with interference.

## How to use this repository?

First get the repository and the associated files (RItools and the Bibliography database):

```
git clone https://github.com/jwbowers/TestStatRIInterference.git
git submodule init
git submodule update
```

Then use the Makefile to build the paper. One R library that we use to paralleize the code, `Rmpi`, assumes either that you are using our local linux cluster (keeling) or you have installed the openmpi libraries via `brew install openmpi` on an OS X machine. We hope that the R package, `Rmpi`, becomes less system dependent to install in the future. 

Some of the simulations take a very long time (more than 24 hours) on a cluster
with 36 cores. We save the products of these runs in a Dropbox directory.
