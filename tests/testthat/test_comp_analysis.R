context("Comparative analysis")

test_that("We can extract tree and trait data to run fitContinuous and fitDiscrete", {
  skip_if_not_installed("geiger")
  
  ## tests are too slow for CRAN
  skip_on_cran()

  nexml <- read.nexml(system.file("examples", "comp_analysis.xml", package="RNeXML"))
  traits <- get_characters(nexml)
  tree <- get_trees(nexml)
  expect_is(tree, "phylo")

  
})


test_that("We can serialize tree and trait data for a comparative analysis", {
  ## tests are too slow for CRAN
  skip_on_cran()
  
  skip_if_not_installed("geiger")

  require("geiger")
  data(geospiza)
  add_trees(geospiza$phy)
  nexml <- add_characters(geospiza$dat)
  write.nexml(nexml, file = "geospiza.xml")
  expect_true_or_null(nexml_validate("geospiza.xml"))
  unlink("geospiza.xml")
})

