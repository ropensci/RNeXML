require("XML")

setAs("XMLInternalElementNode", "node", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "meta", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "edge", function(from) xmlToS4(from))

setAs("XMLInternalElementNode", "tree",
       function(from) {
         obj = new("tree")
         kids = xmlChildren(from)
         obj@edges = new("ListOfedge", lapply(kids[names(kids) == "edge"], as, "edge"))
         obj@nodes = new("ListOfnode", lapply(kids[names(kids) == "node"], as, "edge"))
         ats = xmlAttrs(from, addNamespacePrefix = TRUE)
         for(i in names(ats))
            slot(obj, i) = ats[i]
         obj
       })

## Can be also URL or single file name
filename <- "tests/examples/trees.xml"

doc <- xmlParse(filename)

edges <- getNodeSet(doc, "//x:tree[@id = 'tree1']/x:edge", namespaces="x")
edges <- lapply(edges, xmlToS4)

nodes <- getNodeSet(doc, "//x:tree[@id = 'tree1']/x:node", namespaces="x")
nodes <- lapply(nodes, xmlToS4)

XML:::xmlAttrsToDataFrame(getNodeSet(doc, "//x:tree[@id = 'tree1']/x:node", namespaces="x"))
XML:::xmlAttrsToDataFrame(getNodeSet(doc, "//x:tree[@id = 'tree1']/x:edge", namespaces="x"))


## Usage: nexmldoc <- read.doc(filename)
#read.doc <- function(filename) {
#   
#    return xmlParse(filename, getDTD=FALSE, useInternalNodes = TRUE)
#}


## Usage: doc.release(nexmldoc)
#doc.release <- function(doc) {
#
#    free(doc)
#}


## Usage: trees <- readTrees(doc)
#readTrees <- function(doc) {
#   
#    return readNeXML(doc, "tree")
#}
 

## Read DOM nodes into set of NeXML.Node
#readNeXML <- function(doc, type=c("all", "tree", "taxa", "character")) {

## In progress
##TODO (check what necessary to return)

# Returns a list containing:
#  names of the taxa (from taxa block, implied or declared)
#  names of the trees
#  set of Tree nodes
#  data type for each character block of the nexml file
#  number of characters in each block
#  the labels for the characters

#}



##TODO: methods to process nodes

#setMethod("nNodes", signature(x="Node"), function(x) {
#})

#setMethod("nodeType", signature(x="Node"), function(x) {
#})

#setMethod("nodeId", signature(x="Node"),
# function(x, type=c("trees", "taxes","characters")) {
#})

