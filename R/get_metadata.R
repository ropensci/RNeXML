
## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...

#' get_metadata
#' 
#' get_metadata 
#' @param nexml a nexml object
#' @param level the name of the level of element desired, see details
#' @param simplify logical, see Details
#' @return the requested metadata as a data.frame. Additional columns
#' indicate the parent element of the return value.
#' @details 'level' should be either the name of a child element of a NeXML document 
#' (e.g. "otu", "characters"), or a path to the desired element, e.g. 'trees/tree'
#' will return the metadata for all phylogenies in all trees blocks.
#' 
#' If a metadata element has other metadata elements nested within it, the
#' nested metadata are returned as well. A column "Meta" will contain the
#' IDs consolidated from the type-specific LiteralMeta and ResourceMeta
#' columns, and IDs are generated for meta elements that have nested elements
#' but do not have an ID ("blank nodes"). A column "meta" contains the
#' IDs of the parent meta elements for nested ones. This means that the
#' resulting table can be self-joined on those columns.
#' 
#' If `simplify` is `FALSE`, the type-specific "LiteralMeta" and "ResourceMeta"
#' columns will be retained even if a consolidated "Meta" column is present.
#' Otherwise, only the consolidated column will be included in the result.
#' Also, if `simplify` is `TRUE` the values for "property" (LiteralMeta) and
#' "rel" (ResourceMeta) will be consolidated to "property", and "rel" will be
#' removed from the result.
#' 
#' @import XML
#' @examples \dontrun{
#' comp_analysis <- system.file("examples", "primates.xml", package="RNeXML")
#' nex <- nexml_read(comp_analysis)
#' get_metadata(nex)
#' get_metadata(nex, "otus/otu")
#' }
#' @export
get_metadata <- function(nexml, level = "nexml", simplify = TRUE){
  
#  level = c("nexml", "otus", "trees", "characters", 
#            "otus/otu", "trees/tree", "characters/format", "characters/matrix",
#            "characters/format/states")
#  level <- match.arg(level)

  ## Handle deprecated formats
  if(level =="otu")
    level <- "otus/otu"
  if(level =="tree")
    level <- "trees/tree"

  
  
  if(level == "nexml")
    level <- "meta"
  else
    level <- paste(level, "meta", sep="/") 
 
  out <- get_level(nexml, level)
  if (simplify) {
    cnames = colnames(out)
    if (! ("Meta" %in% cnames)) {
      out <- dplyr::mutate(out,
                           "Meta" = coalesce_(out$LiteralMeta,
                                              out$ResourceMeta,
                                              as.character(rep(NA, times=nrow(out)))))
    }
    out <- dplyr::mutate(out, LiteralMeta = NULL, ResourceMeta = NULL)
    if (all(c("rel","property") %in% cnames))
      out <- dplyr::mutate(out,
                           property = dplyr::if_else(is.na(out$property),
                                                     out$rel,
                                                     out$property),
                           rel = NULL)
    else if ("rel" %in% cnames)
      out <- dplyr::rename(out, property = "rel")
  }

  out
}

#' Get the value(s) for metadata
#'
#' Extracts the values from the metadata annotations for the given property
#' or properties, and returns the result.
#'
#' For matching property identifiers (i.e., URIs), prefixes in the input list
#' as well as in the `annotated` object will be expanded using the namespaces
#' of the `nexml` object. Names in the returned vector are mapped to the
#' (possibly prefixed) form in the input list.
#' @param nexml a nexml object
#' @param annotated the nexml component object from which to obtain metadata
#'   annotations, defaults to the nexml object itself
#' @param props a character vector of property names for which to extract
#'   metadata annotations
#' @return a named character vector, giving the values and names being the
#'   property names
#' @export
get_metadata_values <- function(nexml, annotated = NULL, props){
  metaList <- get_meta(nexml, annotated = annotated, props = props)
  if (length(metaList) > 0) {
    sapply(metaList,
           function(m)
             if (is(m, "LiteralMeta")) charzero_as_empty(m@content) else m@href)
  } else
    c()
}

#' Extracts meta objects matching properties
#'
#' Extracts the metadata annotations for the given property or properties,
#' and returns the result as a list of `meta` objects.
#'
#' For matching property identifiers (i.e., URIs), prefixes in the input list
#' as well as in the `annotated` object will be expanded using the namespaces
#' of the `nexml` object. Names in the returned list are mapped to the
#' (possibly prefixed) form in the input list. The resulting list is flat,
#' and hence does not retain the nesting hierarchy in the object's annotation. 
#' @param nexml a nexml object
#' @param annotated the nexml component object from which to obtain metadata
#'   annotations, or a list of such objects. Defaults to the nexml object itself.
#' @param props a character vector of property names for which to extract
#'   metadata annotations
#' @return a named list of the matching meta objects
#' @export
get_meta <- function(nexml, annotated = NULL, props){
  if (is.null(annotated)) annotated <- nexml
  if (is.null(props) || length(props) == 0)
    stop("Parameter 'props' must be a non-empty vector")
  uris <- expand_prefix(props, nexml@namespaces)
  if (is(annotated, "Annotated"))
    metaList <- get_all_meta(annotated)
  else if (is(annotated, "list"))
    metaList <- unlist(lapply(annotated, get_all_meta))
  else
    stop("'annotated' must be either an instance of 'Annotated', or a list")
  metaProps <- expand_prefix(sapply(metaList, slot, "property"), nexml@namespaces)
  isMatch <- metaProps %in% uris
  if (any(isMatch)) {
    values <- metaList[isMatch]
    mapToURIs <- match(metaProps, uris)
    names(values) <- props[mapToURIs[! is.na(mapToURIs)]]
    New("ListOfmeta", values)
  } else
    New("ListOfmeta")
}

#' Get flattened list of meta annotations
#'
#' Collects recursively (in the case of nested meta annotations) all meta
#' object annotations for the given object, and returns the result as a flat
#' list.
#'
#' Does not check that the input object can actually have meta annotations.
#' An invalid slot error will be generated if it can't.
#' @param annotated the object from which to extract meta object annotations
#' @return a flat list of `meta` objects
#' @export
get_all_meta <- function(annotated) {
  metaList <- slot(annotated, "meta")
  if (length(metaList) > 0) {
    containsNested <- sapply(metaList, is, "ResourceMeta")
    if (any(containsNested)) {
      nested <- lapply(metaList[containsNested], get_all_meta)
      if (length(nested) > 0)
        metaList <- c(metaList, nested)
    }
  }
  metaList
}
