
## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...

#' get_metadata
#' 
#' get_metadata 
#' @param nexml a nexml object
#' @param level the name of the level of element desired, see details
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
#' @import XML
#' @examples \dontrun{
#' comp_analysis <- system.file("examples", "primates.xml", package="RNeXML")
#' nex <- nexml_read(comp_analysis)
#' get_metadata(nex)
#' get_metadata(nex, "otus/otu")
#' }
#' @export
get_metadata <- function(nexml, level = "nexml"){
  
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
 
  get_level(nexml, level)
  

}
