context("taxonomy")


data(bird.orders)
birdorders_small <- drop.tip(bird.orders, tip = 1:10)
birds <- add_trees(birdorders_small)

data(chiroptera)
chiroptera_small <- drop.tip(chiroptera, tip = 1:906)
chir <- add_trees(chiroptera_small)
chiroptera_super_small <- drop.tip(chiroptera, tip = 1:911)
chir_super_small <- add_trees(chiroptera_super_small)





test_that("taxize_nexml correctly collects ncbi identifiers", {
  
  testthat::skip_on_cran()
  
  birds <- taxize_nexml(birds, "NCBI")
  
  chir <- taxize_nexml(chir, "NCBI")
  chir_super_small <- taxize_nexml(chir_super_small, "NCBI")
  
  
  expect_is(birds@otus, "ListOfotus")
  
  expect_is(birds@otus@.Data[[1]]@otu, "ListOfotu")
  expect_is(birds@otus@.Data[[1]]@otu[[1]], "otu")
  expect_is(birds@otus@.Data[[1]]@otu[[1]]@meta, "ListOfmeta")
  expect_is(birds@otus@.Data[[1]]@otu[[1]]@meta[[1]], "meta")
  
  expect_equal(slot(birds@otus@.Data[[1]]@otu[[1]]@meta[[1]], "href"), 
               "http://ncbi.nlm.nih.gov/taxonomy/56308")
  expect_equal(slot(birds@otus@.Data[[1]]@otu[[1]]@meta[[1]], "rel"), 
               "tc:toTaxon")
  
  expect_is(chir@otus, "ListOfotus")
  expect_is(chir@otus@.Data[[1]]@otu, "ListOfotu")
  expect_is(chir@otus@.Data[[1]]@otu[[1]], "otu")
})

test_that("we can extract taxonomy data from the object", {
  
  expect_is(get_metadata(birds, "otus/otu"), "data.frame")
  expect_is(get_metadata(chir_super_small, 'otus/otu'), "data.frame")
})

### TODO: how to deal with missing meta slot elements???


test_that("taxize_nexml throws appropriate warnings", {
  
  testthat::skip_on_cran()
  
  
  chir1 <- drop.tip(chiroptera, tip = 1:910)
  chir1 <- add_trees(chir1)
  expect_warning(taxize_nexml(chir1, "NCBI"))
  
  chiroptera_super_small <- drop.tip(chiroptera, tip = 1:912)
  chir_super_small <- add_trees(chiroptera_super_small)
  expect_is(taxize_nexml(chir_super_small, "NCBI"), "nexml")
  # note from Scott: above test used to test that a warning 
  # was not thrown, but not() is no longer exported from testthat
  # so instead testing that the object returned is of 
  # class nexml
})
