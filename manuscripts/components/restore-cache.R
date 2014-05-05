#!/usr/bin/R

# restore cache file

setwd("components")
archive <- "RNeXML-manuscript-cache.tar.gz"
download.file(paste0("http://two.ucdavis.edu/~cboettig/data/", archive), archive)
untar(archive)
unlink(archive)
setwd("..")


