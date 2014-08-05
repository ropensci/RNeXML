expect_true_or_null <- function(o){
  if(!is.null(o)){
    expect_true(o)
  } else {
    expect_null(o)
  }
}

if (packageVersion("testthat") >= "0.7.1.99") {
    library(testthat)
  test_check("RNeXML")
} else {
  library(testthat)
  test_package("RNeXML")
}

