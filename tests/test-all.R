if (packageVersion("testthat") >= "0.7.1.99") {
  library(testthat)
  test_check("RNeXML")
} else {
  library(testthat)
  test_package("RNeXML")
}

