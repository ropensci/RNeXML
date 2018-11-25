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

  f <- system.file("examples", "biophylo.xml", package="RNeXML")
  nex <- nexml_read(f)

  expect_gt(length(get_namespaces(nex)), length(get_namespaces(nexml())))
  ## Check that the new abbreviations are added
  expect_true(all(c("concept", "map") %in% names(get_namespaces(nex))))

  g <- tempfile()
  nexml_write(nex, g)
  expect_true_or_null(nexml_validate(g))

  nex2 <- nexml_read(g)
  ## check the namespaces remain there
  expect_gt(length(get_namespaces(nex2)), length(get_namespaces(nexml())))
  ## Check that the new abbreviations are added 
  expect_true(all(c("concept", "map") %in% names(get_namespaces(nex2))))
  expect_equal(get_namespaces(nex2)["concept"], get_namespaces(nex)["concept"])
  expect_equal(get_namespaces(nex2)["map"], get_namespaces(nex)["map"])

  f <- system.file("examples", "phenoscape.xml", package = "RNeXML")
  nex <- nexml_read(f)
  # check that the cdao namespace didn't get clobbered
  expect_true("cdao" %in% names(get_namespaces(nex)))
  expect_equivalent(get_namespaces(nex)["cdao"],
                    "http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#")
  expect_true(get_namespaces(nex)["cdao"] != get_namespaces(nexml())["cdao"])
})
