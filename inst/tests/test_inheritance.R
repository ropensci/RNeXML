context("inheritance")

## FIXME
## Should include expect_that tests, rather than just running without errors.  
## ADD test to show that toggling xml->s4->xml returns IDENTICAL objects, 
## Add tests to check values on some nodes/attributes...

test_that("we can perform simple conversions between NeXML XML and S4", {
require(XML)
require(RNeXML)
# basic example
node <- newXMLNode("meta", 
                   attrs = c(id="dict1",
                             property="cdao:has_tag",
			                       content="true", 
                             'xsi:type'="nex:LiteralMeta",
			                       datatype="xsd:boolean"),
                   suppressNamespaceWarning=TRUE)
n2 <- newXMLNode("node", 
                 attrs = c(id = "n4", 
                           label="n4", 
                           about="#n4"), 
                 .children = node)

# check conversions to/from NeXML
 s4 <- as(n2, "node")
 as(s4, "XMLInternalNode")
})



## Should be separate unit tests here to facilitate debugging...

test_that("We can parse a complete NeXML file and toggle back and forth between XML and S4"{
  doc <- xmlParse(system.file("examples", "trees.xml", package="RNeXML"))
  root <- xmlRoot(doc)

# check conversions
  otu <- as(root[["otus"]][[1]], "otu")
  as(otu, "XMLInternalNode")

  trees <- as(root[["trees"]], "trees")
  as(trees, "XMLInternalNode")

  otus <- as(root[["otus"]], "otus")
  as(otus, "XMLInternalNode")


  parsed <- as(root, "nexml")
  serialized <- as(parsed, "XMLInternalNode")
})
