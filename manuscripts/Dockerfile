FROM cboettig/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org
## Remain current
RUN apt-get update -qq && apt-get dist-upgrade -y

## LaTeX dependencies for building the manuscript
RUN apt-get install -y texlive-humanities lmodern texlive-fonts-recommended texlive-latex-extra

## This is stupidly large but easiest way to grab the various .sty files in use...
# RUN apt-get install texlive-full

## Could skip this, but installs binary versions which is much faster
RUN apt-get install -yq r-cran-taxize r-cran-ape r-cran-phytools r-cran-uuid r-cran-plyr r-cran-reshape2 r-cran-phylobase r-cran-geiger r-cran-rfigshare r-cran-testthat r-cran-knitcitations
## Install package, with dependencies and suggests
RUN Rscript -e 'devtools::install_github("ropensci/RNeXML")'

RUN apt-get install wget
RUN wget http://www.phytools.org/nonstatic/phytools_0.4-27.tar.gz
RUN R CMD INSTALL phytools_0.4-27.tar.gz


## Clone the repo and switch into manscripts directory
RUN git clone http://github.com/ropensci/RNeXML.git /home/rstudio/RNeXML
RUN chmod u+rw,g+rw,o+rw /home/rstudio/RNeXML
WORKDIR /home/rstudio/RNeXML/manuscripts

## Compile the manuscript & supplement
RUN Rscript -e 'rmarkdown::render("manuscript.Rmd")'
RUN Rscript -e 'rmarkdown::render("supplement.Rmd")'


