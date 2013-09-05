context("top level API")


test_that("read.nexml works (to ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)

  f <- system.file("examples", "trees.xml", package="RNeXML")
  phy <- nexml_read(f, type="phylo")
#  phy <- read.nexml(f, type="phylo")
  layout(matrix(1:2, 1, 2), c(5, 4))
  plot(phy[[1]])
  plot(phy[[2]])
})

test_that("write.nexml works (from ape::phylo)", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)
  library(XML)

  data(bird.orders)
  nexml_write(bird.orders, file="example.xml")

## Check that that example is valid NeXML
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)
  unlink("example.xml") # cleanup

})


