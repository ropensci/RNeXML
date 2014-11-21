FROM rocker/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

RUN mkdir /home/rstudio
RUN git clone https://github.com/ropensci/RNeXML.git /home/rstudio/rnexml
RUN Rscript -e 'devtools::install("/home/rstudio/rnexml")'

## Allows us to run using RStudio's non-root owner
RUN chown -R docker:docker /home/rstudio/rnexml

## build manuscript:
# docker run --name rnexml --rm -it -w /home/rstudio/rnexml/mansucripts cboettig/rnexml Rscript -e 'rmarkdown::render("manuscript.Rmd")'
# docker copy rnexml:/home/rstudio/rnexml/manuscripts/manuscript.pdf .
