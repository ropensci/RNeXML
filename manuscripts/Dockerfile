FROM cboettig/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

## Could skip this, but installing these binary versions is much faster
## Also, some of these won't install properly from cran at the moment...
RUN apt-get install -yq r-cran-taxize r-cran-ape r-cran-phytools r-cran-uuid r-cran-plyr r-cran-phylobase r-cran-geiger r-cran-rfigshare r-cran-testthat r-cran-knitcitations

RUN Rscript -e 'devtools::install_github("ropensci/RNeXML", dependencies=TRUE)'

## Install the latest version of phytools, since it's not yet on CRAN (or Github)
RUN apt-get install wget
RUN wget http://www.phytools.org/nonstatic/phytools_0.4-27.tar.gz
RUN R CMD INSTALL phytools_0.4-27.tar.gz

## Clone the repository onto the image
RUN git clone https://github.com/ropensci/RNeXML.git /host/RNeXML

## Cannot add parent directory with the package itself this way, only the current dir/subdirs
## ADD . /host/RNeXML/manuscripts

## Run tests: make sure we can compile the manuscript & supplement without errors
WORKDIR /host/RNeXML/manuscripts
RUN Rscript -e 'setwd(".."); devtools::install(); devtools::check()'
RUN Rscript -e 'rmarkdown::render("manuscript.Rmd"); rmarkdown::render("supplement.Rmd")'


