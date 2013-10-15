context("meta")


library(ape)
library(RNeXML)
library(XML)
data(bird.orders)

test_that("We can add license and pubdate by default in metadata", {
  ## The short version using an RNeXML API
  nexml_write(bird.orders, file="example.xml")
  nex <- nexml_read("example.xml", "nexml")
  
  expect_equal(nex@meta[[1]]@property[[1]], "dc:date")
  expect_equal(nex@meta[[2]]@rel[[1]], "cc:license") 
  unlink("example.xml") # cleanup

})

test_that("We can add additional metadata", {
  ## The short version using an RNeXML API

  nexml_write(bird.orders, file="meta_example.xml",
              title = "My test title",
              description = "A description of my test",
              creator = "Carl Boettiger <cboettig@gmail.com>",
              publisher = "unpublished data",
              pubdate = "2012-04-01")
  
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "meta_example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

##  Add some tests actually checking content is correct
  nex <- nexml_read("meta_example.xml", "nexml")

  unlink("meta_example.xml") # cleanup

})


test_that("We can add R bibentry type metadata", {
  ## The short version using an RNeXML API

  nexml_write(bird.orders, file="example.xml", citation=citation("ape")) 
  nex <- nexml_read("example.xml", "nexml")

  require(XML) 
  ## xmlParse and check with xpath
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

  unlink("example.xml") # cleanup

})



test_that("We can add arbitrary metadata", {
  ## The short version using an RNeXML API

  history <- new("meta", 
      content = "Mapped from the bird.orders data in the ape package using RNeXML",
      datatype = "xsd:string", id = "meta5144", property = "skos:historyNote", 
      'xsi:type' = "LiteralMeta")
  modified <- new("meta",
                  content = "2013-10-04", datatype = "xsd:string", id = "meta5128",
                  property = "prism:modificationDate", 'xsi:type' = "LiteralMeta")
  website <- new("meta", 
                 href = "http://carlboettiger.info", 
                 rel = "foaf:homepage", 'xsi:type' = "ResourceMeta")
  nexml_write(bird.orders, 
              file = "example.xml", 
              additional_metadata = list(history, modified, website), 
              additional_namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                        prism = "http://prismstandard.org/namespaces/1.2/basic/",
                                        foaf = "http://xmlns.com/foaf/0.1/"))

  
  nex <- nexml_read("example.xml", "nexml")

  require(XML) 
  ## xmlParse and check with xpath
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

  unlink("example.xml") # cleanup

})





test_that("We can add arbitrary metadata", {
  ## The short version using an RNeXML API

  rdfa <- '<div typeof="foaf:Person" about="http://carlboettiger.info#me">
             <a rel="foaf:account" href="https://twitter.com/cboettig">twitter</a> 
             <a rel="foaf:account" href="https://github.com/cboettig">github</a>
           </div>'
  parsed <- xmlRoot(xmlParse(rdfa))
  arbitrary_rdfa <- meta(property="eml:additionalMetadata", content="additional metadata", children = parsed)
  nexml_write(bird.orders, 
            file = "example.xml", 
            additional_metadata = list(arbitrary_rdfa), 
            additional_namespaces = c(foaf = "http://xmlns.com/foaf/0.1/", 
                                      eml = "eml://ecoinformatics.org/eml-2.1.1"))

  
  nex <- nexml_read("example.xml", "nexml")

  require(XML) 
  ## xmlParse and check with xpath
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

  unlink("example.xml") # cleanup

})



















