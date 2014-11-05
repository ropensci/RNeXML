FROM rocker/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org
RUN git clone https://github.com/ropensci/RNeXML.git /rnexml
RUN Rscript -e 'devtools::install("/rnexml")'


#RUN Rscript -e 'devtools::check("/rnexml")'

## build manuscript:
# docker run --name rnexml --rm -it -w /rnexml/mansucripts cboettig/rnexml Rscript -e 'rmarkdown::render("manuscript.Rmd")'
# docker copy rnexml:/rnexml/manuscripts/manuscript.pdf .
