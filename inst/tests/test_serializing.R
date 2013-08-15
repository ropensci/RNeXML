context("serializing")

test_that("We can serialize ape to S4 RNeXML into valid NeXML",{
  library(XML)
  library(RNeXML)
  library(ape)
  data(bird.orders)
  tree <- as(bird.orders, "tree") 

  nexml_write(tree, "test.xml")

# results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "test.xml")
  results <- xmlSchemaValidate("~/Documents/code/thirdparty/nexml/xsd/nexml.xsd", "test.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

 ##  Clean up
  unlink("test.xml")

  })


test_that("We can serialize ape to S4 RNeXML into valid NeXML",{
  library(XML)
  library(RNeXML)
  root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
  tree <- as(root[["trees"]][["tree"]], "tree")
  nexml_write(tree, "test.xml")

# results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "test.xml")
  results <- xmlSchemaValidate("~/Documents/code/thirdparty/nexml/xsd/nexml.xsd", "test.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

 ##  Clean up
  unlink("test.xml")

  })


