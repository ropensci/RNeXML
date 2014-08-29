FROM cboettig/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

## Could skip this, but installing these binary versions is much faster
#RUN apt-get install -yq r-cran-taxize r-cran-ape r-cran-phytools r-cran-uuid r-cran-plyr r-cran-phylobase r-cran-geiger r-cran-rfigshare r-cran-testthat r-cran-knitcitations

## Install this package itself, along with dependencies and suggests
RUN apt-get install -yq r-cran-ape r-cran-expm r-cran-taxize
RUN Rscript -e 'devtools::install_github("ropensci/RNeXML", dependencies=TRUE)'

## Install the latest version of phytools, since it's not yet on CRAN (or Github)
RUN apt-get install wget
RUN wget http://www.phytools.org/nonstatic/phytools_0.4-27.tar.gz
RUN R CMD INSTALL phytools_0.4-27.tar.gz

## Adds the contents of the working directory onto the image
ADD . /tmp/RNeXML/manuscripts

## Run tests: make sure we can compile the manuscript & supplement without errors
WORKDIR /tmp/RNeXML/manuscripts
RUN R CMD check ..
RUN Rscript -e 'rmarkdown::render("manuscript.Rmd"); rmarkdown::render("supplement.Rmd")'


