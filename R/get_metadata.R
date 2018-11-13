
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
#' If `simplify` is `TRUE`, the type-specific "LiteralMeta" and "ResourceMeta"
#' columns will be removed if a consolidated "Meta" column is present. The
#' values for "property" (LiteralMeta) and "rel" (ResourceMeta) will be
#' consolidated to "property", and "rel" will be removed.
#' 
#' @import XML
#' @examples \dontrun{
#' comp_analysis <- system.file("examples", "primates.xml", package="RNeXML")
#' nex <- nexml_read(comp_analysis)
#' get_metadata(nex)
#' get_metadata(nex, "otus/otu")
#' }
#' @export
get_metadata <- function(nexml, level = "nexml", simplify = FALSE){
  
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
    if ("Meta" %in% cnames)
      out <- dplyr::select_(out, quote(-LiteralMeta), quote(-ResourceMeta))
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
