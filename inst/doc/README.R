## ----compile-settings, include=FALSE-------------------------------------
## Set the paths for cache and figure
library(methods)
library(knitr)
opts_chunk$set(cache = 1)
basename <- gsub(".Rmd", "", knitr:::knit_concord$get('infile')) 
opts_chunk$set(fig.path = paste("figure/", basename, "-", sep=""),
               cache.path = paste("cache/", basename, "/", sep=""))



## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_knit$set(upload.fun = imgur_upload)
opts_chunk$set(tidy=FALSE, warning=FALSE, message=FALSE, comment = NA, verbose = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  library(devtools)
#  install_github("RNeXML", "ropensci")
#  library(RNeXML)

## ----echo=FALSE----------------------------------------------------------
library(RNeXML)

## ------------------------------------------------------------------------
f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nexml <- nexml_read(f)
tr <- get_trees(nexml) # or: as(nexml, "phylo")
plot(tr)

## ------------------------------------------------------------------------
data(bird.orders)
nexml_write(bird.orders, "test.xml")

## ------------------------------------------------------------------------
nexml_validate("test.xml")

## ------------------------------------------------------------------------
birds <- nexml_read("test.xml")
get_taxa(birds)
get_metadata(birds) 

## ------------------------------------------------------------------------
  nexml_write(bird.orders, file="meta_example.xml",
              title = "My test title",
              description = "A description of my test",
              creator = "Carl Boettiger <cboettig@gmail.com>",
              publisher = "unpublished data",
              pubdate = "2012-04-01")


## ------------------------------------------------------------------------
modified <- meta(property = "prism:modificationDate",
                 content = "2013-10-04")

## ------------------------------------------------------------------------
RNeXML:::nexml_namespaces

## ------------------------------------------------------------------------
website <- meta(href = "http://carlboettiger.info", 
                rel = "foaf:homepage")

## ------------------------------------------------------------------------
  history <- meta(property = "skos:historyNote", 
                  content = "Mapped from the bird.orders data in the ape package using RNeXML",
                  id = "meta123")

## ------------------------------------------------------------------------
  birds <- add_trees(bird.orders)
  birds <- add_meta(meta = list(history, modified, website),
                    namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                   foaf = "http://xmlns.com/foaf/0.1/"),
                    nexml=birds)
  nexml_write(birds, 
              file = "example.xml")
              


## ----eval=FALSE----------------------------------------------------------
#  nex <- add_trees(bird.orders)
#  nex <- taxize_nexml(nex)

## ------------------------------------------------------------------------
library(RNeXML)
nexml <- read.nexml(system.file("examples", "comp_analysis.xml", package="RNeXML"))
traits <- get_characters(nexml)
tree <- get_trees(nexml)

## ------------------------------------------------------------------------
library(geiger)
fitContinuous(tree, traits[1], ncores=1)
fitDiscrete(tree, traits[2], ncores=1)

## ----include=FALSE-------------------------------------------------------
unlink("example.xml")
unlink("meta_example.xml")
unlink("test.xml")

