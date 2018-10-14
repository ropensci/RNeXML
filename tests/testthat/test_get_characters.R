context("get_characters")

test_that("Getting characters", {

  f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
  nex <- read.nexml(f)
  out <- get_characters(nex)
  expect_is(out, "data.frame")
  
})

test_that("get_characters can deal with polymorphic character states", {
  f <- system.file("examples", "ontotrace-result.xml", package="RNeXML")
  nex <- read.nexml(f)
  m <- get_characters(nex,
                      rownames_as_col = TRUE,
                      otu_id = TRUE,
                      otus_id = TRUE)
  expect_is(m, "data.frame")
  expect_gt(ncol(m), 3)
  expect_true(any(is.na(m[, 4])))
  expect_false(all(is.na(m[, 4])))
  expect_is(m[, 4], "integer")
  expect_is(m[, ncol(m)], "character")
  expect_false(any(is.na(m[, ncol(m)])))
  expect_gt(max(nchar(m[, ncol(m)])), 1)
})
