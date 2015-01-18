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
##     docker cp rnexml:/home/rstudio/rnexml/manuscript.pdf .
## 
## Equivalent to:
##  
##     docker run --name rnexml --volumes $(pwd):/home/rstudio/rnexml \
##       --workdir /home/rstudio/rnexml --user rstudio rocker/ropensci \
##       R -e "rmarkdown::render('manuscript.Rmd')"
## 
##
## Interactive use: 
## 
##    docker run -d -p 8787:8787 -u 0 cboettig/RNeXML supervisord
##
## and then login at http://localhost:8787 with user:pw rstudio:rstudio
