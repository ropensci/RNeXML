context("serializing")

library(XML)
library(RNeXML)

## More tests at lower-level serializing from S4 to XML in inheritance.R

test_that("We can serialize ape to S4 RNeXML into valid NeXML",{
  library(ape)
  data(bird.orders)


  nexml <- as(bird.orders, "nexml") 

  as(nexml, "XMLInternalNode")
  ###  Higher level API tests
  nexml_write(bird.orders, "test.xml")

 results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "test.xml")
#  results <- xmlSchemaValidate("~/Documents/code/thirdparty/nexml/xsd/nexml.xsd", "test.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

 ##  Clean up
  unlink("test.xml")

  })


test_that("We can serialize parsed NeXML to S4 RNeXML into valid NeXML",{
  root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
  tree <- as(root, "nexml")
  nexml_write(tree, "test.xml")

#! change directory to something that will work across machines if possible
  
# results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "test.xml")
  results <- xmlSchemaValidate("~/Documents/code/thirdparty/nexml/xsd/nexml.xsd", "test.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

 ##  Clean up
  unlink("test.xml")

  })



#root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
#tree <- as(root, "nexml")
#tree@trees[[1]]@tree[[1]]@node[[4]]@meta
#as(root[["trees"]][["tree"]][[4]][["meta"]], "meta")


