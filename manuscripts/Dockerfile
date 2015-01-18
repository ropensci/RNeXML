FROM rocker/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

COPY . /home/rstudio/rnexml
RUN chown -R rstudio:rstudio /home/rstudio/rnexml
WORKDIR /home/rstudio/rnexml
USER rstudio
CMD ["R", "-e", "rmarkdown::render('manuscript.Rmd')"]


## Non-interactive use:
## 
##     docker run --name rnexml cboettig/rnexml
##     docker cp rnexml:/home/rstudio/rnexml/manuscript.pdf `pwd`

## Equivalent to:
##  
##     docker run --name rnexml --volume=$(pwd):/home/rstudio/rnexml \
##       --workdir /home/rstudio/rnexml --user docker rocker/ropensci \
##       R -e "rmarkdown::render('manuscript.Rmd')"
## 
##
## Interactive use: 
## 
##    docker run -dP cboettig/RNeXML supervisord
