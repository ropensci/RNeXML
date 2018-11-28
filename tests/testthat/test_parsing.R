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

test_that("base namespace gets added if not present", {
  doc <- xmlParse(system.file("examples", "no-base-ns.xml", package="RNeXML"))
  xmlroot <- xmlRoot(doc)

  prefixes <- names(xmlNamespaceDefinitions(doc))
  expect_false(any(prefixes == ""))

  nex <- nexml_read(doc)
  expect_true(any(names(get_namespaces(nex)) == ""))
  expect_equal(expand_prefix("/nexml", get_namespaces(nex)),
               expand_prefix("/nexml", get_namespaces(nexml())))
  expect_equal(expand_prefix("nexml", get_namespaces(nex)),
               expand_prefix("nex:nexml", get_namespaces(nex)))
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
