FROM cboettig/ropensci-docker:latest
MAINTAINER Carl Boettiger cboettig@ropensci.org


# Install package, with dependencies and suggests
RUN apt-get install r-cran-rjava

RUN Rscript -e 'options(repos=c(CRAN="http://cran.rstudio.com")); devtool::install_github("ropensci/RNeXML")'

# Clone the repo and switch into manscripts directory
RUN git clone http://github.com/ropensci/RNeXML.git /tmp/RNeXML
WORKDIR /tmp/RNeXML/manuscripts

# Compile the manuscript
RUN Rscript -e 'rmarkdown::render("manuscript.Rmd")
RUN Rscript -e 'rmarkdown::render("supplement.Rmd")


