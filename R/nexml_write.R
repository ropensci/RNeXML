#' Write nexml files
#' 
#' @param x any phylogeny object (e.g. phylo, phylo4, or internal type)
#' @param file the name of the file to write out
#' @param additional_metadata list of
#' @param add_identifiers either logical or name of an identifier (see details or \code{\link{add_identifiers}})
#'        If TRUE, default identifier (NCBI) will be added.  If FALSE, no identifier is added.  
#' @return Writes out a nexml file
#' @import ape 
#' @import XML
#' @aliases nexml_write write.nexml
#' @export nexml_write write.nexml
#' @examples
#' 
#'  library(ape); data(bird.orders)
#'  nexml_write(bird.orders, file="example.xml")
#' 
#'  ## Adding citation to a publication
#'  nexml_write(bird.orders, file="example.xml",
#'              citation=citation("ape"))
#' 
#'  ## Adding custom metadata 
#'  history <- meta(content="Mapped from the bird.orders data in the ape package using RNeXML",
#'                  datatype="xsd:string", 
#'                  id="meta5144", 
#'                  property="skos:historyNote")
#'  modified <- meta(content="2013-10-04", datatype="xsd:string", id="meta5128",
#'                  property="prism:modificationDate")
#'  website <- meta(href="http://carlboettiger.info", rel="foaf:homepage")
#'  nexml_write(bird.orders, 
#'              file = "example.xml", 
#'              additional_metadata = list(history, modified, website), 
#'              additional_namespaces = c(skos="http://www.w3.org/2004/02/skos/core#",
#'                                        prism="http://prismstandard.org/namespaces/1.2/basic/",
#'                                        foaf = "http://xmlns.com/foaf/0.1/"))
nexml_write <- function(x, 
                        file = NULL, 
                        title = NULL, 
                        description = NULL,
                        creator = NULL,
                        pubdate = Sys.Date(),
                        rights = "CC0",
                        publisher = NULL,
                        citation = NULL,
                        additional_metadata = NULL,
                        additional_namespaces = NULL,
                        add_identifiers = TRUE){
  nex <- as(x, "nexml")

  ## FIXME Check for duplicates first. Only a duplicate if prefix is also duplicated.  
  nex@namespaces = c(nex@namespaces, additional_namespaces)

  if(is.logical(add_identifiers))
    if(add_identifiers)
      add_identifiers = "NCBI"
  if(is.character(add_identifiers))
    nex <- addIdentifiers(nex, type = add_identifiers)
  out <- as(nex, "XMLInternalNode")
  ## FIXME Kind of strange to add these to the XML representation instead of directly to the S4 nexml representation....
  ## would be easy to add to a ListOfmeta...
  if(!is.null(additional_metadata))
      addChildren(out, kids = additional_metadata, at = 0)
  if(!is.null(description))
      addChildren(out, description(description), at = 0)
   if(!is.null(citation)){
     citations <- nexml_citation(citation)
     if(is.list(citations[[1]])){
      for(entry in citations) 
        addChildren(out, kids = entry, at = 0)
     } else if(is(citations[[1]], "meta")){
       addChildren(out, kids = citations, at = 0)
     }
  }
  if(!is.null(rights))
      addChildren(out, rights(rights), at = 0)
  if(!is.null(publisher))
      addChildren(out, publisher(publisher), at = 0)
  if(!is.null(pubdate))
      addChildren(out, pubdate(pubdate), at = 0)
  if(!is.null(creator))
      addChildren(out, creator(creator), at = 0)
  if(!is.null(title))
      addChildren(out, title(title), at = 0)


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


