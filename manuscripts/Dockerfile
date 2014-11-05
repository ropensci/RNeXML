FROM rocker/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

## Clone the repository onto the image
RUN git clone https://github.com/ropensci/RNeXML.git /host/RNeXML

## Run tests: make sure we can compile the manuscript & supplement without errors
WORKDIR /host/RNeXML/manuscripts
RUN Rscript -e 'setwd(".."); devtools::install(); devtools::check()'
RUN Rscript -e 'rmarkdown::render("manuscript.Rmd"); rmarkdown::render("supplement.Rmd")'


