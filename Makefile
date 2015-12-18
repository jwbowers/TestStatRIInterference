# Standard Makefile boilerplate
SHELL = /bin/sh

.SUFFIXES:

RCMD = R --vanilla

# Sweave is always run in a subdir

Sweave = R_LIBS=../.libraries $(RCMD) CMD Sweave

TEX2PDF = latexmk -pdf

INSTALLLIB = $(RCMD) CMD INSTALL --no-multiarch --no-docs --library=.libraries
installpkg = mkdir -p .libraries ; R_LIBS=.libraries R --vanilla -e "install.packages('$(1)', repos = 'http://streaming.stat.iastate.edu/CRAN/')" ; date > .libraries/$(1)/INSTALLED
	LIBRARIES = .libraries/.d .libraries/RItools/INSTALLED .libraries/packages.txt

MAKEFIG = cd figures && $(Sweave)

all: paper.pdf

### Paper Targets
# note: the figures only included at the latex step, so the individual
# sections do not need to be rebuilt when the figures change, but we notate
# the logical flow by tabbing in for the figures that used in each section
paper.pdf: figures/twoDplots.pdf \
  BIB/big.bib \
  styles/common.sty \
  styles/notation.sty \
  paper/paper.tex \
  paper/introduction.tex \
  paper/titlepage.tex \
  coppock-replication/CoppockJEPS_figure2.pdf
	cd paper && $(TEX2PDF) paper.tex
	cp paper/paper.pdf .

# a faster build than the paper, but less exact for figures
draft:
	SEARCH_POINTS=10 REPETITIONS=100 make paper.pdf


# Libraries

.libraries/allpkgs.txt: .libraries/packages.txt .libraries/RItools/INSTALLED

.libraries/.d:
	mkdir -p .libraries
	touch .libraries/.d

.libraries/packages.txt: .libraries/.d
	R_LIBS=".libraries" $(RCMD) --file=pkgs/packages.R
	echo "Packages last updated on" `date` > .libraries/packages.txt

R: $(LIBRARIES)
	R_LIBS=".libraries" $(RCMD) --no-save --quiet

pkgs/RItools:
	git submodule init && git submodule update

.libraries/RItools/INSTALLED: pkgs/RItools/R/* \
  .libraries/SparseM/INSTALLED .libraries/xtable/INSTALLED .libraries/svd/INSTALLED .libraries/abind/INSTALLED .libraries/hexbin/INSTALLED .libraries/brew/INSTALLED
	mkdir -p .libraries
	cd pkgs/RItools && make package
	$(INSTALLLIB) pkgs/RItools/$(shell cd pkgs/RItools && make current)
	date > .libraries/RItools/INSTALLED

# the following are RItools dependencies: svd, abind, xtable, SparseM
.libraries/SparseM/INSTALLED:
	$(call installpkg,SparseM)

.libraries/xtable/INSTALLED:
	$(call installpkg,xtable)

.libraries/svd/INSTALLED:
	$(call installpkg,svd)

.libraries/abind/INSTALLED:
	$(call installpkg,abind)

.libraries/hexbin/INSTALLED:
	$(call installpkg,hexbin)

.libraries/brew/INSTALLED:
	$(call installpkg,brew)

.libraries/maptools/INSTALLED:
	$(call installpkg,maptools)

.libraries/spdep/INSTALLED:
	$(call installpkg,spdep)

.libraries/wnominate/INSTALLED:
	$(call installpkg,wnominate)
	### Paper components

### Tables and Figures

figures/teststat2dfigs.txt: figures/teststat-parallel-summary.R \
  simulation/teststat-parallel-results.rda \
  code/plotting.R
	R_LIBS=".libraries" $(RCMD) --file=figures/teststat-parallel-summary.R

figures/twoDplots.pdf: figures/teststat2dfigs.txt

figures/simulation-network-graph.tex: $(LIBRARIES) code/plotting.R simulation/setup.R figures/simulation-network-graph.Rnw
	$(MAKEFIG) simulation-network-graph.Rnw

#### Simulations Galore ####
simulation/teststat-parallel.rda: $(LIBRARIES) simulation/setup.R simulation/teststat-parallel.R
	R_LIBS=".libraries" $(RCMD) --file=simulation/teststat-parallel.R

simulation/teststat-parallel-results.rda: simulation/teststat-parallel.rda
	R_LIBS=".libraries" $(RCMD) --file=simulation/teststat-parallel-results.R


### Two cleaning tasks. The default clean does not remove *.rda files, deepclean does
clean:
	git ls-files --others --exclude="*rda*" --exclude=".libraries" | xargs rm -rf

deepclean:
	git clean -xdf

## Right now my idea is to run git ls-files --others > filestodelete.txt and then add that list to the archive and feel it to rm
## below.
filestodelete.txt:
	git ls-files --others > filestodelete.txt
	echo ".libraries" >> filetodelete.txt

deepclean-repo: filestodelete.txt
	cat filestodelete.txt | xargs rm -rf

### Export Tasks
# the archive export includes the .Rnw files so the people can tweak the code if they wish
ARCHIVENAME=teststatistics-randomization-inference-interference
$(ARCHIVENAME).tar.gz: Makefile README.md filestodelete.txt paper/* simulation/* code/* BIB/* figures/* styles/*
	rm -rf $(ARCHIVENAME)
	mkdir $(ARCHIVENAME)
	rsync -a --exclude-from=.gitignore --exclude=.git* \
	  . $(ARCHIVENAME)
	tar -cz $(ARCHIVENAME) > $(ARCHIVENAME).tar.gz

archive: $(ARCHIVENAME).tar.gz

# the static export dumps all the .tex, .pdf, etc files needed for paper.pdf
$(ARCHIVENAME)-static.tar.gz: paper.pdf
	rm -rf $(ARCHIVENAME)-static
	mkdir $(ARCHIVENAME)-static
	grep INPUT paper/paper.fls | grep -v texlive | sed "s/^INPUT /paper\//" > $(ARCHIVENAME)-static/MANIFEST
	rsync --files-from=$(ARCHIVENAME)-static/MANIFEST . $(ARCHIVENAME)-static
	sed "s/\\\\bibliographystyle.*//" $(ARCHIVENAME)-static/paper/paper.tex  | sed "s/\\\\bibliography.*/\\\\input{paper.bbl}/" > paper.tmp
	mv paper.tmp $(ARCHIVENAME)-static/paper/paper.tex
	echo "#!/bin/sh\ncd paper\nlatexmk -pdf -bibtex- paper.tex\ncp paper.pdf ..\n" > $(ARCHIVENAME)-static/build.sh
	chmod +x $(ARCHIVENAME)-static/build.sh
	tar -cz $(ARCHIVENAME)-static > $(ARCHIVENAME)-static.tar.gz

static: $(ARCHIVENAME)-static.tar.gz

#### Replication of Coppock (2014)

coppock-replication/CoppockJEPS_10000Randomizations.rdata: coppock-replication/CoppockJEPS_10000Randomizations.R coppock-replication/nm.replication.tab \
  .libraries/spdep/INSTALLED .libraries/maptools/INSTALLED .libraries/wnominate/INSTALLED
	cd coppock-replication && R_LIBS=../.libraries $(RCMD) -f CoppockJEPS_10000Randomizations.R

coppock-replication/CoppockJEPS.rdata: coppock-replication/CoppockJEPS_10000Randomizations.rdata coppock-replication/CoppockJEPS_datapreparation.R
	cd coppock-replication && R_LIBS=../.libraries $(RCMD) -f CoppockJEPS_datapreparation.R

coppock-replication/fig2.rdata: code/teststatistics.R coppock-replication/CoppockJEPS_figure2code.R coppock-replication/CoppockJEPS.rdata
	cd coppock-replication && R_LIBS=../.libraries $(RCMD) -f CoppockJEPS_figure2code.R

coppock-replication/CoppockJEPS_figure2.pdf: coppock-replication/fig2.rdata coppock-replication/CoppockJEPS_figure2graphic.R
	cd coppock-replication && R_LIBS=../.libraries $(RCMD) -f CoppockJEPS_figure2graphic.R

coppock-replication/CoppockJEPS_figure3a.pdf: coppock-replication/CoppockJEPS.rdata coppock-replication/CoppockJEPS_figure3code.R
	cd coppock-replication && R_LIBS=../.libraries $(RCMD) -f CoppockJEPS_figure3code.R
