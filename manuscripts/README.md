Instructions for compiling manuscripts
======================================

[![Build Status](http://107.170.225.143:88/api/badge/github.com/ropensci/RNeXML/status.svg?branch=master)](http://107.170.225.143:88/github.com/ropensci/RNeXML)


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
by running RStudio in a container.  This (a) avoids having to install
software dependencies, and (b) avoids altering anything on your local
library. If the above doesn't work, or just for fun, give this a try.

Installation: In a Mac or Windows machine, this will aslo install boot2docker
(easy point & click install, ~24 MB). On Linux, this installs
natively and we can run everything in the terminal instead of in
the boot2docker window.
([[Mac](https://docs.docker.com/installation/mac/)],
[[Windows](https://docs.docker.com/installation/windows/),
[[Linux](https://docs.docker.com/installation)]).


1) Launch boot2docker, and do:

```bash
sudo docker run -d -p 8787:8787 cboettig/rnexml
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
login with user/password `rstudio/rstudio`, and be able to run stuff,
install packages, use git, etc.



