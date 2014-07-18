context("Online validator tool")
library(RNeXML)
test_that("example file validates", {
  f <- system.file("examples", "trees.xml", package="RNeXML")
  RNeXML:::expect_true_or_null(nexml_validate(f)) # null if we cannot perform validation, don't fail
})

test_that("RNeXML-generated file validates", {
  data(bird.orders)
  f <- nexml_write(bird.orders, file="test.xml") 
  RNeXML:::expect_true_or_null(nexml_validate(f))
  unlink("test.xml")
})

