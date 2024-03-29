#' Write nexml files
#' 
#' @param x a nexml object, or any phylogeny object (e.g. phylo, phylo4) 
#' that can be coerced into one. Can also be omitted, in which case a new 
#' nexml object will be constructed with the additional parameters specified.
#' @param file the name of the file to write out
#' @param trees phylogenetic trees to add to the nexml file (if not already given in x)
#' see \code{\link{add_trees}} for details.  
#' @param characters additional characters
#' @param meta A meta element or list of meta elements, see \code{\link{add_meta}}
#' @param ... additional arguments to add__basic_meta, such as the title.  See \code{\link{add_basic_meta}}.   
#' @return Writes out a nexml file
#' @import ape
#' @import XML 
#' @import methods
#' @aliases nexml_write write.nexml
#' @export nexml_write write.nexml
#' @seealso \code{\link{add_trees}} \code{\link{add_characters}} \code{\link{add_meta}} \code{\link{nexml_read}}

#' @examples
#'  ## Write an ape tree to nexml, analgous to write.nexus:
#'  library(ape); data(bird.orders)
#'  ex <- tempfile(fileext=".xml")
#'  write.nexml(bird.orders, file=ex)
nexml_write <- function(x = nexml(),
                        file = NULL,
                        trees = NULL,
                        characters = NULL,
                        meta = NULL, 
                        ...){
  
  nexml <- as(x, "nexml")
  if(!is.null(trees))
    nexml <- add_trees(trees, nexml = nexml)
  if(!is.null(characters))
    nexml <- add_characters(characters, nexml = nexml)
  if(!is.null(meta))
    nexml <- add_meta(meta, nexml = nexml)
  argList <- list(..., nexml = nexml)
  # set last modification time upon writing, if not set already
  if (is.null(argList$pubdate)) argList$pubdate <- Sys.time()
  nexml <- do.call(add_basic_meta, argList)
  
  out <- as(nexml, "XMLInternalNode")
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
  nexml(
      trees = New("ListOftrees", list(trees)),
      otus = otus)
})


setAs("ListOfnode", "otus", function(from) nexml.otus(otu = from))

setAs("tree", "trees", function(from)
  nexml.trees(tree = New("ListOftree", list(from))))


