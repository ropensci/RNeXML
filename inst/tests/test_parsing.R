context("parsing")

# More lower-level parsing tests in inheritance

test_that("We can parse a NeXML file to an S4 RNeXML::tree object", {
  library(RNeXML)
  library(XML)
  f <- system.file("examples", "trees.xml", package="RNeXML")
  doc <- xmlParse(f)
  root <- xmlRoot(doc)
  nexml <- as(root, "nexml")  ## parse the XML into S4
  expect_is(nexml,"nexml")
})


