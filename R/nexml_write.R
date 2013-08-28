#' Write nexml files
#' 
#' @aliases write.nexml
#' @param x any phylogeny object (e.g. phylo, phylo4, or internal type)
#' @param file the name of the file to write out
#' @return Writes out a nexml file
#' @import ape 
#' @import XML
#' @export
nexml_write <- function(x, file = "nexml.xml"){
  out <- as(as(x, "nexml"), "XMLInternalDocument")
  saveXML(out, file = file)
}




############## Promotion methods ########
## FIXME -- Coercion is not the way to go about any of this


## want generator methods that can handle id creation better
# consider:
# setMethod("promote", 
#           signature("tree", "character"),
#           function(object, target_type)

setAs("tree", "nexml", function(from){
  trees = as(from, "trees")
  otus = as(from, "otus")
  otus@id = "tax1" #UUIDgenerate()
  trees@id = "Trees" #UUIDgenerate()
  trees@otus = otus@id
  new("nexml", 
      trees = new("ListOftrees", list(trees)),
      otus = otus)
})

#setAs("tree", "otus", function(from){
#  nodes_with_otus <- 
#    plyr::compact(sapply(from@node, 
#                         function(n) if(length(n@otu > 0)) n))
#  new("otus", otu=new("ListOfotu", lapply(nodes_with_otus, as, "otu")))
#})

setAs("ListOfnode", "otus", function(from)
  new("otus", otu = from))

#setAs("node", "otu", function(from)
#  new("otu", id = unname(from@otu)))


setAs("tree", "trees", function(from)
  new("trees", tree = new("ListOftree", list(from))))

setAs("XMLInternalNode", "XMLInternalDocument", 
      function(from) newXMLDoc(node = from))

setAs("nexml", "XMLInternalDocument", function(from)
  as(as(from, "XMLInternalNode"), "XMLInternalDocument"))


