context("Comparative analysis")


library(RNeXML)
library(geiger)


test_that("We can extract tree and trait data to run fitContinuous and fitDiscrete", {
  nexml <- read.nexml(system.file("examples", "comp_analysis.xml", package="RNeXML"))
  traits <- get_characters_list(nexml)
  tree <- get_tree(nexml)
  expect_is(tree, "phylo")
  expect_is(traits[[1]], "data.frame")
  cts <- fitContinuous(tree, traits[[1]])
  dte <- fitDiscrete(tree, traits[[2]])
})


test_that("We can serialize tree and trait data for a comparative analysis", {
  data(geospiza)
  add_trees(geospiza$phy)
  nexml <- add_characters(geospiza$dat)
  write.nexml(nexml, "geospiza.xml")
  expect_true(nexml_validate("geospiza.xml"))
  unlink("geospiza.xml")
})

