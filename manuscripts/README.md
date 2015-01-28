Instructions for compiling manuscripts
======================================

[![Build Status](http://server.carlboettiger.info:88/api/badge/github.com/ropensci/RNeXML/status.svg?branch=master)](http://server.carlboettiger.info:88/github.com/ropensci/RNeXML)

[![Circle CI](https://circleci.com/gh/ropensci/RNeXML.svg?style=svg)](https://circleci.com/gh/ropensci/RNeXML)


Install dependencies
--------------------

Install the dependencies required for the supplementary examples using the `devtools` package:

```r
install.packages("devtools")
devtools::install_github(c("egonw/rrdf/rrdflibs", "egonw/rrdf/rrdf", "cboettig/Sxslt"))
```

Then install the `RNeXML` R package, including the suggested packages, using the following R command:

```r
install.packages("RNeXML", dependencies=TRUE)
```

Note that `rmarkdown` requires `pandoc` (>= 0.12.3) and `pandoc-citeproc` be installed. These ship with the current version of RStudio (`>=0.98`). Additionally, a LaTeX environment is required to generate the output pdf. 



Build the manuscript
--------------------


Make sure you set the `manuscripts/` as your working directory and then do:

```r
rmarkdown::render("manuscript.Rmd")
```
or use the `knit2pdf` button in your RStudio console. 

Alternately: Using Docker
-------------------------

Instead of installing R packages seperately, you can try out RNeXML
by running RStudio in a Docker container.  This (a) avoids having to install
software dependencies, and (b) avoids altering anything on your local
library. If the above doesn't work, or just for fun, give this a try.

The `RNeXML` package and all dependencies are installed on the [rocker/ropensci](http://registry.hub.docker.com/u/rocker/ropensci) Docker container.  You will still need all
the files from this directory (the `manuscripts` directory on the RNeXML Github repository)
to build the manuscript. Users can decide to run either an R console (accessed through a terminal) 
or an RStudio instance (accessed through the browser) on the container. 



### Docker Installation

In a Mac or Windows machine, this will aslo install boot2docker
(easy point & click install, ~24 MB). On Linux, this installs
natively and we can run everything in the terminal instead of in
the boot2docker window.
([[Mac](https://docs.docker.com/installation/mac/)],
[[Windows](https://docs.docker.com/installation/windows/),
[[Linux](https://docs.docker.com/installation)]).

### R console

With boot2docker running, run `R` on the `rocker/ropensci` image,
linking the location of your copy of this directory to
`/home/rstudio` on the container, setting the container's
working directory to the same, and setting user as `rstudio`:

```bash
docker run -v /path/to/RNeXML/manuscripts:/home/rstudio \ 
  -w /home/rstudio -u rstudio -ti --rm rocker/ropensci R
```

At the R prompt, you can use `rmarkdown` to render the manuscript PDF from the `Rmd` file:

```r
rmarkdown::render('manuscript.Rmd')
```

`manuscript.pdf` should now be created in the manuscripts directory.  

### Using RStudio

1) From the command line (with boot2docker running on Mac/Windows), do:

```bash
sudo docker run -d -p 8787:8787 rocker/ropensci
```

That will take a while to download the image the first time you run it.

2) Once it is done, try:

```bash
boot2docker ip
```
that should return an ip address you can paste into your browser.

3) Add a `:8787` to the end of this address and paste it into your
browser address bar. (e.g. it's probably `http://92.168.59.103:8787`
but that can change).

4) You should get the RStudio welcome screen.  you should be able to
login with user/password `rstudio/rstudio`.

5) Clone the RNeXML repository using New Project from Version Control (https://github.com/ropensci/RNeXML), switch into the `manuscripts` directory and you should be good to go as above.  

