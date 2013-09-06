#' Write nexml files
#' 
#' @param x any phylogeny object (e.g. phylo, phylo4, or internal type)
#' @param file the name of the file to write out
#' @return Writes out a nexml file
#' @import ape 
#' @import XML
#' @aliases nexml_write write.nexml
#' @export nexml_write write.nexml
nexml_write <- function(x, file = "nexml.xml"){
  out <- as(as(x, "nexml"), "XMLInternalNode")
  saveXML(out, file = file)
}

write.nexml <- nexml_write


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


setAs("ListOfnode", "otus", function(from)
  new("otus", otu = from))

setAs("tree", "trees", function(from)
  new("trees", tree = new("ListOftree", list(from))))


