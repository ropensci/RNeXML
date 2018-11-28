
#' Add namespaces
#' 
#' Add namespaces and their prefixes as a named vector of URIs, with the
#' names being the prefixes. Namespaces have most relevance for meta objects'
#' `rel` and `property`, and for embedded XML literals.
#'
#' The implementation attempts to avoid duplication, currently using the
#' prefix. I.e., namespaces with prefixes already defined will not get added.
#' Namespaces needed by the NeXML format, and for commonly used metadata
#' terms, are already included by default, see [get_namespaces()].
#' @note Often a user won't call this directly, but instead provide the
#'   namespace(s) through [add_meta()].
#' @param namespaces a named character vector of namespaces
#' @param nexml a nexml object. will create a new one if none is given.  
#' @return a nexml object with updated namespaces 
#' @examples
#' ## Write multiple metadata elements, including a new namespace:  
#' website <- meta(href = "http://carlboettiger.info", 
#'                 rel = "foaf:homepage")     # meta can be link-style metadata
#' modified <- meta(property = "prism:modificationDate",
#'                  content = "2013-10-04")
#' nex <- add_meta(list(modified,  website), 
#'                 namespaces = c(foaf = "http://xmlns.com/foaf/0.1/"))
#'                 # prism prefix already included by default
#'
#' ## Add namespace "by hand" before adding meta:
#' nex <- add_namespaces(c(skos = "http://www.w3.org/2004/02/skos/core#"),
#'                       nexml = nex)
#' history <- meta(property = "skos:historyNote",
#'                 content = "Mapped from the bird.orders data in the ape package using RNeXML")
#' nex <- add_meta(history, nexml = nex)
#' 
#' @seealso [meta()] [add_meta()] [get_namespaces()]
#' @export 
add_namespaces <- function(namespaces, nexml = new("nexml")){
  if(!is.null(namespaces)){
## check for duplicated abbreviation, not for duplicated URI. OKAY to have multiple abbrs for same URI...

## FIXME Make sure that cases where abbreviation match actually match the URI as well
    notdups <- match(names(namespaces), names(nexml@namespaces)) 
    notdups <- sapply(notdups, is.na)
    if(all(notdups)) # all are unique 
      nexml@namespaces <-  c(nexml@namespaces, namespaces)
    else {
      nexml@namespaces <-  c(nexml@namespaces, namespaces[notdups])
    } 
  }
  nexml
}
