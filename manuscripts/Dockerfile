FROM rocker/ropensci:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org

COPY . /host
RUN chown -R docker:docker /host
WORKDIR /host
USER docker
CMD ["R", "-e", "rmarkdown::render('manuscript.Rmd')"]


## Non-interactive use:
## 
##     docker run --name rnexml cboettig/rnexml
##     docker cp rnexml:/host/manuscript.pdf .

## Equivalent to:
##  
##     docker run --name rnexml --volumes $(pwd):/host \
##       --workdir /host --user docker rocker/ropensci \
##       R -e "rmarkdown::render('manuscript.Rmd')"
## 
##
## Interactive use: 
## 
##    docker run -dP cboettig/RNeXML supervisord
