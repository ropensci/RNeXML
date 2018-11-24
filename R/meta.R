## Utilities for adding additional metadata

#' @name nexml.meta_
#' @aliases nexml.meta_
#' @seealso [nexml.meta()][meta()] for documentation of `nexml.meta()`
#' @rdname constructors
NULL

#' Constructor function for metadata nodes
#' 
#' @param property specify the ontological definition together with it's namespace, e.g. dc:title
#' @param content content of the metadata field
#' @param rel Ontological definition of the reference provided in href 
#' @param href A link to some reference
#' @param datatype optional RDFa field
#' @param id optional id element (otherwise id will be automatically generated).  
#' @param type optional xsi:type.  If not given, will use either "LiteralMeta" or "ResourceMeta" as 
#'  determined by the presence of either a property or a href value. 
#' @param children Optional element containing any valid XML block (XMLInternalElementNode class, see the XML package for details).  
#' @details User must either provide property+content or rel+href.  Mixing these will result in potential garbage. 
#' The datatype attribute will be detected automatically from the class of the content argument.  Maps from R class
#' to schema datatypes are as follows: 
#' character - xs:string, 
#' Date - xs:date,
#' integer - xs:integer,
#' numeric - xs:decimal,
#' logical - xs:boolean
#' 
#' @examples
#' meta(content="example", property="dc:title")
#' @export 
#' @seealso \code{\link{nexml_write}}
#' @aliases nexml.meta
#' @include classes.R
#' @importFrom plyr compact
meta <- function(property = NULL,
                 content = NULL,
                 rel = NULL,
                 href = NULL,
                 datatype = NULL,
                 id = NULL,
                 type = NULL,
                 children = list()){
  # if datatype isn't provided, try to infer it from content
  if (is.null(datatype) && !is.null(content)) {
    if(is.logical(content))
      datatype <- "xsd:boolean"
    else if(is(content, "Date"))
      datatype <- "xsd:date"
    else if(is.numeric(content))
      datatype <- "xsd:decimal"
    else if(is.character(content) && length(content) > 0)
      datatype <- "xsd:string"
    else if(is.integer(content))
      "xsd:integer"
  }
  # the content text must be written as a string
  if (length(content) > 0)
    content <- as.character(content)

  if(length(id) == 0)
    id <- nexml_id("m")
   
  if(is(children, "XMLAbstractNode") || is(children, "XMLInternalNode"))
    children <- list(children)

  # if type is not provided, try to determine it from the parameters
  if (is.null(type)) {
    if (length(rel) > 0)
      type <- "ResourceMeta"
    else if (length(href) > 0 ||
             (length(children) > 0 && all(sapply(children, is, "meta")))) {
      type <- "ResourceMeta"
      rel <- property
      property <- NULL
    } else if (length(property) > 0)
      type <- "LiteralMeta"
  }
  # if null is provided for value, don't create any object
  if (length(children) == 0 &&
      ((is.null(content) && type == "LiteralMeta") ||
       (is.null(href) && type == "ResourceMeta")))
    NULL
  else {
    # determine the meta class to instantiate
    clname <- type
    if (is.null(type)) {
      clname <- "meta"
      # we have to default to some xsi:type that makes sense
      type <- "LiteralMeta"
    }
    # create the meta instance
    args <- plyr::compact(list(property = property,
                               content = content,
                               datatype = datatype,
                               rel = rel,
                               href = href,
                               id = id,
                               'xsi:type' = type,
                               children = children))
    do.call(New, c(clname, args))
  }
}

nexml.meta <- meta

## Common helper functions 

#' @importFrom plyr compact
nexml_citation <- function(obj){
  if(is(obj, "BibEntry"))
    class(obj) <- "bibentry"
  if(is(obj, "bibentry")){
    out <- lapply(obj, function(obj){
      if(length(grep("--", obj$pages)) > 0){
        pgs <- strsplit(obj$pages, "--")[[1]]
        start_page <- pgs[[1]]
        end_page <- if(length(pgs)>1) pgs[[2]] else " "
      } else if(length(grep("-", obj$pages)) > 0){
        pgs <- strsplit(obj$pages, "-")[[1]]
        start_page <- pgs[[1]]
        end_page <- if(length(pgs)>1) pgs[[2]] else " "
      } else {
        start_page <- NULL
        end_page <- NULL
      }
      list_of_metadata_nodes <- plyr::compact(c(list(
        meta(content=obj$volume, 
            property="prism:volume"),
        meta(content=obj$journal, 
            property="dc:publisher"),
        meta(content=obj$journal, 
            property="prism:publicationName"),
        meta(content = end_page, 
            property="prism:endingPage"),
        meta(content=start_page, 
            property="prism:startingPage"),
        meta(content=obj$year, 
            property="prism:publicationDate"),
        meta(content=obj$title,
            property="dc:title")),
        lapply(obj$author, function(x){
          meta(content = format(x, c("given", "family")),
               property="dc:contributor")
          }),
        meta(content=format(obj, "text"), 
             property="dcterms:bibliographicCitation")
      ))
      citation_elements = New("ListOfmeta", list_of_metadata_nodes)
      meta(rel = "dcterms:references",
           children = citation_elements)
    })
    out 
  }
}



#' Concatenate meta elements into a ListOfmeta
#' 
#' Concatenate meta elements into a ListOfmeta
#' @param x,... `meta` and `ListOfmeta` elements to be concatenated, see \code{\link{meta}}
#' @param recursive  logical, if 'recursive=TRUE', the function recursively
#'   descends through lists and combines their elements into a flat vector.
#'   This method does not support `recursive=FALSE`, use [list][base::list()]
#'   instead.
#' @return a ListOfmeta object containing a flat list of meta elements.
#' @examples 
#' c(meta(content="example", property="dc:title"),
#'   meta(content="Carl", property="dc:creator"))
#' @rdname c-meta
#' @aliases c-meta
#' @include classes.R
#' @importFrom plyr compact
setMethod("c", 
          signature("meta"),
          function(x, ..., recursive = TRUE){
            elements <- list(x, ...)
            if (identical(recursive, FALSE))
              stop("Use list() to concatenate 'meta' and 'ListOfmeta' non-recursively")
            new("ListOfmeta", unlist(elements))

          })


#' Concatenate ListOfmeta elements into a ListOfmeta
#' 
#' Concatenate ListOfmeta elements into a flat ListOfmeta
#' @inheritParams c-meta
#' @examples 
#' metalist <- c(meta(content="example", property="dc:title"),
#'               meta(content="Carl", property="dc:creator"))
#' out <- c(metalist, metalist) 
#' out <- c(metalist, meta(content="a", property="b")) 
#' @rdname c-meta
#' @aliases c-ListOfmeta
#' @importFrom plyr compact
setMethod("c", 
          signature("ListOfmeta"),
          function(x, ..., recursive = TRUE){
            elements <- list(x, ...)
            if (identical(recursive, FALSE))
              stop("Use list() to concatenate 'meta' and 'ListOfmeta' non-recursively")
            new("ListOfmeta", plyr::compact(unlist(elements)))

          })

setGeneric("slot")
setGeneric("slot<-")

#' Access or set slot of S4 object
#'
#' See [methods::slot()]. This version allows using "property" consistently
#' for both LiteralMeta and ResourceMeta (which internally uses "rel" because
#' RDFa does), which is easier to program. It also allows using "meta"
#' as an alias for "children" for ResourceMeta, to be consistent with the
#' corresponding slot for instances of `Annotated`.
#' @param object the object
#' @param name name of the slot
#' @aliases slot-ResourceMeta
#' @seealso [methods::slot()]
#' @export
setMethod("slot",
          signature("ResourceMeta", "ANY"),
          function(object, name) {
            if (name == "property")
              object@rel
            else if (name == "meta")
              object@children
            else
              callNextMethod()
          })

#' @param value the new value
#' @rdname slot-ResourceMeta-method
#' @export
setMethod("slot<-",
          signature("ResourceMeta", "ANY"),
          function(object, name, value) {
            if (name == "property")
              object@rel <- value
            else if (name == "meta")
              object@children <- value
            else
              object <- callNextMethod()
            object
          })
