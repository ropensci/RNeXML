context("ape")


test_that("From ape::phylo to RNeXML::nexml object", {
   library(RNeXML)
   library(ape)
   data(bird.orders)
   as(bird.orders, "nexml")

})



test_that("We can go from various orderings of ape::phylo to RNeXML::nexml", {
  library(RNeXML)
  library(ape)
  data(bird.orders)
  nexml <- as(bird.orders, "nexml")
  phy <- as(nexml, "phylo")

  ## Demonstrate that we now have a phylo object
  library(ape)
  plot(phy)
})


test_that("From nexml to multiPhylo", {

  library(RNeXML)

  # part of base testing, could be replaced with higher level, but why 
  library(XML)
  f <- system.file("examples", "trees.xml", package="RNeXML")
  doc <- xmlParse(f)
  root <- xmlRoot(doc)
  nexml <- as(root, "nexml")  ## parse the XML into S4

  ## APE TEST:  Coerce the S4 into phylo S3 object
  phy <- as(nexml, "phylo")
  expect_is(phy, "multiPhylo")

})


## This unit test is really not testing ape functions but just the higher-level nexml_write function...
test_that("We can serialize the various versions of the ape format", {
  library(RNeXML)
  library(ape)
  data(bird.orders)
  nexml <- as(bird.orders, "nexml")
  nexml_write(nexml, "test.xml")
  unlink("test.xml")
})

