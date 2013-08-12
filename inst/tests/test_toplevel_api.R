context("top level API")


test_that("read.nexml works (to ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)

  f <- system.file("examples", "tree.xml", "RNeXML")
  phy <- nexml_read(f, type="phylo")
#  phy <- read.nexml(f, type="phylo")
  plot(phy)
})

test_that("write.nexml works (from ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)
  library(XML)

  data(bird.orders)
  nexml_write(bird.orders, file="example.xml")

## Check that that example is valid NeXML
  xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  unlink("example.xml") # cleanup

})


