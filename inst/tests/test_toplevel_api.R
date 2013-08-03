context("top level API")


test_that("read.nexml works (to ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)

  f <- system.file("examples", "tree.xml", "RNeXML")
  phy <- read.nexml(f, as="phylo")
  plot(phy)
})

test_that("write.nexml works (from ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)

  phy <- data(bird.orders)
  write.nexml(phy, file="example.xml")

## Check that that example is valid NeXML
  xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  unlink("example.xml") # cleanup

})


