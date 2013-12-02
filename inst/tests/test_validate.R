context("Online validator tool")
library(RNeXML)
test_that("example file validates", {
  f <- system.file("examples", "trees.xml", package="RNeXML")
  expect_true(nexml_validate(f))
})

test_that("RNeXML-generated file validates", {
  data(bird.orders)
  f <- nexml_write(bird.orders, file="test.xml") 
  expect_true(nexml_validate(f))
  unlink("test.xml")
})

