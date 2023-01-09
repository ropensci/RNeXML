context("Online validator tool")
test_that("example file validates", {
  
  ## tests are too slow for CRAN
  skip_on_cran()
  f <- system.file("examples", "trees.xml", package="RNeXML")
  expect_true_or_null(nexml_validate(f)) # null if we cannot perform validation, don't fail
})

test_that("RNeXML-generated file validates", {
  ## tests are too slow for CRAN
  skip_on_cran()
  
  
  data(bird.orders)
  f <- nexml_write(bird.orders, file="test.xml") 
  o <- nexml_validate(f)
  if(!is.null(o)){
    expect_true(o)
  } else {
    expect_null(o)
  }
  unlink("test.xml")
})


test_that("Validation can fail gracefully", {
  
  ## tests are too slow for CRAN
  skip_on_cran()
  
  f <- system.file("examples/sparql.newick", package="RNeXML")
  
  expect_error(
  o <- nexml_validate(f)
  )
})

