
#' Read NeXML files into various R formats
#' 
#' @param x Path to the file to be read in 
#' @param type the type of object to be returned.  If the file 
#' contains multiple trees, all will be read into the appropriate 
#' multi-tree container, or else returned as a list of such objects.  
#' @aliases read.nexml 
#' @import XML
#' @import ape 
#' @export
#' @examples
#' f <- system.file("examples", "trees.xml", package="RNeXML")
#' nexml_read(f) 
nexml_read <- function(x, type = c("phylo", "phylo4", "ouch",
                                   "character_matrix", "nexml")){
  type <- match.arg(type) 
  doc <- xmlParse(x)
  # will return class multiphylo if phylo is asked for and multiple trees are given.  
  as(doc, type)
}


setAs("XMLInternalDocument", "phylo", function(from){
  trees <- getNodeSet(from, "//nex:trees/nex:tree", namespaces="nex")
  out <- lapply(trees, as, "phylo")
  if(length(out) == 1)
    out == out[[1]]
  else if(length(out) > 1)
     class(out) = "multiPhylo"
  else 
    warning("No NeXML trees found in file")
  out
})


### Coercion methods from XML to S4 types, e.g. for reading XML ########## 

setAs("XMLInternalElementNode", "meta", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "node", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "edge", function(from) xmlToS4(from))
#setAs("XMLInternalElementNode", "node", function(from){
#         obj = new("node")
#         kids = xmlChildren(from)
#         obj@meta = new("ListOfmeta", lapply(kids[names(kids) == "meta"], as, "meta"))
#         ats = xmlAttrs(from, addNamespacePrefix = TRUE)
#         for(i in names(ats))
#            slot(obj, i) = ats[i]
#         obj
#       }
#      )
#setAs("XMLInternalElementNode", "edge", function(from) {
#         obj = new("edge")
#         kids = xmlChildren(from)
#         obj@meta = new("ListOfmeta", lapply(kids[names(kids) == "meta"], as, "meta"))
#         ats = xmlAttrs(from, addNamespacePrefix = TRUE)
#         for(i in names(ats))
#            slot(obj, i) = ats[i]
#         obj
#       })
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


