Instructions for compiling manuscripts
======================================




Install dependencies
--------------------

Note that `rmarkdown` requires `pandoc` (>= 0.12) and `pandoc-citeproc` be installed. See [installing pandoc](http://johnmacfarlane.net/pandoc/installing.html). Then from R (with `devtools` R package installed):

```r
devtools::install_github("cboettig/knitcitations@v1")
devtools::install_github("ropensci/RNeXML@devel", dependencies = c("Depends", "Imports", "Suggests"))
devtools::install_github("rstudio/rmarkdown")
```


Build the manuscript
--------------------


If you have the [RStudio Preview release](www.rstudio.com/ide/download/preview) (>= 0.98.932), just open the manuscript.Rmd file in RStudio and try pressing the 'knit pdf' button.

Otherwise, make sure you set the `manuscripts/` as your working directory and then do:

```r
rmarkdown::render("manuscript.Rmd")
```


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



