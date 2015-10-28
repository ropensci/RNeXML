
## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...

#' get_metadata
#' 
#' get_metadata 
#' @param nexml a nexml object
#' @param level the name of the level of element desired, see details
#' @return the requested metadata as a data.frame. Additional columns
#' indicate tha parent element of the return value.
#' @details 'level' should be either the name of a child element of a NeXML document 
#' (e.g. "otu", "characters"), or a path to the desired element, e.g. 'trees/tree'
#' will return the metadata for all phylogenies in all trees blocks.
#' @import XML
#' @examples
#' comp_analysis <- system.file("examples", "primates.xml", package="RNeXML")
#' nex <- nexml_read(comp_analysis)
#' get_metadata(nex)
#' get_metadata(nex, "otus/otu")
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

#' @importFrom dplyr select
get_level <- function(nex, level){
  lvl <- strsplit(level, "/")[[1]]

  ## Trick to apply nodelist_to_df iteratively
  closure <- function(level, fun) function(node) nodelist_to_df(node, level, fun)
  recursion <- function(i, level){
    if(i < length(level))
      closure(level[i], recursion(i+1, level))
    else
      closure(level[i], attributes_to_row)
  }
  
  recursion(1, lvl)(nex) %>% dplyr::select_(quote(-nexml))
}


## Assumes slot(node, element) is a list
#' @importFrom lazyeval interp
#' @importFrom dplyr bind_rows mutate_ %>%
nodelist_to_df <- function(node, element, fn){
  dots <- setNames(list(lazyeval::interp(~x, x = node_id(node))), class(node))
  slot(node, element) %>% 
    lapply(fn) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate_(.dots = dots)
}

node_id <- function(node){
  if("id" %in% slotNames(node))
    slot(node, "id")
  else
    "root"
}

attributes_to_row <- function(node, skip = c("meta", "children")){
  who <- slotNames(node)
  who <- who[-which(who %in% skip)]
  tmp <- sapply(who, function(x) slot(node, x))
  tmp[sapply(tmp,length) < 1] <- NA
  data.frame(as.list(tmp), stringsAsFactors=FALSE)
}


## Depricated method, still in use in some other functions

setxpath <- function(object){
  tmp <- tempfile()
  suppressWarnings(saveXML(object, tmp))
  doc <- xmlParse(tmp)
  unlink(tmp)
  doc
}

