context("parsing")

# More lower-level parsing tests in inheritance

test_that("We can parse a NeXML file to an S4 RNeXML::tree object", {
  f <- system.file("examples", "trees.xml", package="RNeXML")
  doc <- xmlParse(f)
  root <- xmlRoot(doc)
  nexml <- as(root, "nexml")  ## parse the XML into S4
  expect_is(nexml,"nexml")
})



test_that("We preserve existing namespace", {

  f <- system.file("examples/biophylo.xml", package="RNeXML")
  nex <- nexml_read(f)
  g <- tempfile()
  nexml_write(nex, g)

  expect_true_or_null(nexml_validate(g))

  nex2 <- nexml_read(g)

  ## check the namespaces are added 
  expect_gt(length(get_namespaces(nex2)), length(get_metadata(nex)))

  ## Check that the new abbreviations are added 

})


test_that("files with rootedge can be parsed and roundtripped", {
  f <- system.file("examples", "coal.xml", package = "RNeXML")
  nex <- read.nexml(f)

  tr <- nex@trees[[1]]@tree[[1]]
  testthat::expect_true(length(tr@rootedge@target) > 0 && nchar(tr@rootedge@target) > 0)
  testthat::expect_equal(sum(sapply(tr@node, slot, "id") == tr@rootedge@target), 1)
  xmlOut <- as(nex, "XMLInternalNode")

  nex2 <- read.nexml(xmlOut)
  tr2 <- nex2@trees[[1]]@tree[[1]]
  testthat::expect_true(length(tr2@rootedge@target) > 0 && nchar(tr2@rootedge@target) > 0)
  testthat::expect_equal(sum(sapply(tr2@node, slot, "id") == tr2@rootedge@target), 1)
  testthat::expect_equal(sum(sapply(tr2@node, slot, "id") == tr@rootedge@target), 1)
  testthat::expect_equal(saveXML(as(tr, "XMLInternalNode")),
                         saveXML(as(tr2, "XMLInternalNode")))

})
