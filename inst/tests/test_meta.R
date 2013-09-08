
test_that("We can add license and pubdate by default in metadata", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)
  library(XML)

  data(bird.orders)
  nexml_write(bird.orders, file="example.xml")
  nex <- nexml_read("example.xml", "nexml")
  
  expect_equal(nex@meta[[1]]@property[[1]], "dc:date")
  expect_equal(nex@meta[[2]]@rel[[1]], "cc:license") 
  unlink("example.xml") # cleanup

})

test_that("We can add additional metadata", {
  ## The short version using an RNeXML API
  library(ape)
  library(RNeXML)
  library(XML)

  data(bird.orders)
  nexml_write(bird.orders, file="meta_example.xml",
              title = "My test title",
              description = "A description of my test",
              creator = "Carl Boettiger <cboettig@gmail.com>",
              publisher = "unpublished data",
              pubdate = "2012-04-01")
  nex <- nexml_read("meta_example.xml", "nexml")
 
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "meta_example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

  unlink("meta_example.xml") # cleanup

})


