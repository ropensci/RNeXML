context("get_characters")

require(XML)
require(RNeXML)

f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
nex <- read.nexml(f)
char_list <- get_characters_list(nex)

# with different row.names
row.names(char_list[[1]])[1:3] <- c("taxon_18","taxon_20","taxon_30")
out <- get_characters(char_list)

# with suffixes
out2 <- get_characters(char_list, suffixes=TRUE)

# long list
longlist <- rep(char_list, 5)
out3 <- get_characters(longlist, suffixes=TRUE)

# list of 1 data.frame
onedf <- char_list[[1]]
out4 <- get_characters(onedf)

test_that("get_characters returns the correct class", {
  expect_is(get_characters(nex), "data.frame")
  expect_is(char_list, "list")
  expect_is(out, "data.frame")
  expect_is(out2, "data.frame")
  expect_is(out3, "data.frame")
  expect_is(out4, "data.frame")
})

test_that("get_characters returns the correct dimensions", {
  expect_equal(dim(get_characters(nex)), c(10,2))
  expect_equal(length(char_list), 2)
  expect_equal(dim(out), c(13,2))
  expect_equal(dim(out2), c(13,2))
  expect_equal(dim(out3), c(13,10))
  expect_equal(dim(out4), c(10,1))
  
  expect_equal(dim(na.omit(out3)), c(7,10))
  expect_equal(dim(na.omit(out4)), c(10,1))
})

test_that("get_characters throws appropriate warnings", {
  expect_that(get_characters(nex), not(gives_warning()))
  expect_that(get_characters(char_list), not(gives_warning()))
  expect_warning(get_characters(longlist, suffixes=TRUE))
})