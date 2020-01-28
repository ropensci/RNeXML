testthat::context("summary for nexml objects")

test_that("summary() doesn't error on various inputs", {
  
  ## tests are too slow for CRAN
  skip_on_cran()
  
  files <- list.files(path = system.file("examples", package = "RNeXML"),
                      pattern = ".*\\.xml$", full.names = TRUE)
  for (f in files) {
    doc <- XML::xmlParse(f)
    docType <- XML::xmlName(XML::xmlChildren(doc)[[1]])
    if (docType == "nexml") {
      nex <- nexml_read(f)
      s <- summary(nex)
      testthat::expect_is(s, "list")
      testthat::expect_equal(names(s$nblocks), c("trees", "otus", "characters"))
      for (elem in names(s)) {
        if (elem == "nmeta") {
          testthat::expect_is(s[[elem]], "list")
          testthat::expect_length(s[[elem]], 4)
        } else {
          testthat::expect_true(is.integer(s[[elem]]) || is.matrix(s[[elem]]))
        }
      }
      testthat::expect_output(show(nex), regexp = "NeXML")
    }
  }
})

test_that("summary() gives the correct counts", {
  nex <- nexml_read(system.file("examples", "comp_analysis.xml", package = "RNeXML"))
  s <- summary(nex)
  testthat::expect_equivalent(s$nblocks, c(1, 1, 2))
  testthat::expect_true(all(is.na(s$nstates[,1])))
  testthat::expect_true(all(s$nstates[,2] == 2))
  testthat::expect_equivalent(s$nmatrixrows, c(10, 10))
  testthat::expect_equivalent(s$notus, c(10))

  nex <- nexml_read(system.file("examples", "treebase-record.xml", package = "RNeXML"))
  s <- summary(nex)
  testthat::expect_equivalent(s$nblocks, c(1, 1, 1))
  testthat::expect_equal(ncol(s$nstates), 1)
  testthat::expect_true(all(s$nstates[,1] == 4))
  testthat::expect_equivalent(s$nnonstdstatedefs, c(0, 14))
  testthat::expect_equivalent(s$nmatrixrows, c(52))
  testthat::expect_equivalent(s$notus, c(52))

  nex <- nexml_read(system.file("examples", "ontotrace-result.xml", package = "RNeXML"))
  s <- summary(nex)
  testthat::expect_equivalent(s$nblocks, c(1, 1, 1))
  testthat::expect_equivalent(s$ncharacters, c(3))
  testthat::expect_equal(ncol(s$nstates), 1)
  testthat::expect_true(all(s$nstates[,1] == 2))
  testthat::expect_equivalent(s$nnonstdstatedefs, c(2, 0))
  testthat::expect_equivalent(s$nmatrixrows, c(9))
  testthat::expect_equivalent(s$nmeta$otu, c(10))
  testthat::expect_equivalent(s$nmeta$char, c(3))
})
