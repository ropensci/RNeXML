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

test_that("get_characters can return state type matrix", {
  f <- system.file("examples", "ontotrace-result.xml", package="RNeXML")
  nex <- read.nexml(f)
  ret <- get_characters(nex, include_state_types = TRUE)
  expect_is(ret, "list")
  expect_is(ret$characters, "data.frame")
  expect_is(ret$state_types, "data.frame")
  expect_equal(dim(ret$characters), dim(ret$state_types))
  expect_true(all(sapply(ret$state_types, is.factor)))
  expect_gt(sum(ret$state_types == "polymorphic", na.rm = TRUE), 1)
  expect_true(all(is.na(ret$characters) == is.na(ret$state_types)))
})
