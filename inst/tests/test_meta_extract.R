context("extract_metadata")

library(ape)
library(RNeXML)
library(XML)
data(bird.orders)

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
nexml_write(bird.orders, file="example.xml",
            title = "My test title",
            description = "A description of my test",
            creator = "Carl Boettiger <cboettig@gmail.com>",
            publisher = "unpublished data",
            pubdate = "2012-04-01",
            citation = citation("ape"),
            additional_metadata = list(history, modified, website), 
            additional_namespaces = c(skos = "http://www.w3.org/2004/02/skos/core#",
                                      prism = "http://prismstandard.org/namespaces/1.2/basic/",
                                      foaf = "http://xmlns.com/foaf/0.1/"))




test_that("we can extract various metadata", {

  ## FIXME add the appropriate expect_that checks here
  nex <- read.nexml("example.xml")
  get_citation(nex)
  get_license(nex)
  get_metadata(nex)
  summary(nex)

            })



test_that("example parses", {
  require(XML) 
  ## xmlParse and check with xpath
  results <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", "example.xml")
  expect_equal(results$status, 0)
  expect_equal(length(results$errors), 0)

  unlink("example.xml") # cleanup
            })
