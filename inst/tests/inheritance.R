
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


# Parse NeXML and toggle back and forth
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

