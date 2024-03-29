context("Geiger tests (may take 15+ minutes)")


has_geiger <- require(geiger)


test_that("We can write caudata data to nexml", {
  skip_if_not(has_geiger)
  skip_on_cran()
  
  data(caudata)
  nexml_write(trees = caudata$phy, characters = caudata$dat, file="tmp.xml")
  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})


test_that("We can write geospiza data to nexml", {
  skip_on_cran()
  skip_if_not(has_geiger)
  data(geospiza)
  nexml_write(trees = geospiza$phy, characters = geospiza$dat, file="tmp.xml")
  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})

test_that("We can write chelonia data to nexml", {
  skip_on_cran()
  skip_if_not(has_geiger)
  data(chelonia)
  nexml_write(trees = chelonia$phy, characters = chelonia$dat, file="tmp.xml")
  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})

test_that("We can write primates data to nexml", {
  skip_on_cran()
  skip_if_not(has_geiger)
  data(primates)
  nexml_write(trees = primates$phy, characters = primates$dat, file="tmp.xml")
  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})

test_that("We can write whales data to nexml", {
  skip_on_cran()
  skip_if_not(has_geiger)
  data(whales)
# taxa need to be rownames not separate column 
  skip_on_cran()
  whales$dat <- whales$richness[[2]]
  names(whales$dat) <- whales$richness[[1]] 
  nexml_write(trees = whales$phy, characters = whales$dat, file="tmp.xml")
  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})


test_that("We can write amphibia multiphylo to nexml. Two of these phylogenies each have nearly 3K taxa, so this may take around 12 minutes", {
# multiphylo, where two phylogenies have each nearly 3K taxa
  skip_on_cran()
  skip_if_not(has_geiger)
  data(amphibia)
  class(amphibia) <- "multiPhylo"
  runtime <- system.time(nexml_write(amphibia, file="tmp.xml")) # Slow! about 12 minutes

  expect_true_or_null(nexml_validate("tmp.xml"))
  unlink("tmp.xml") # cleanup
})

