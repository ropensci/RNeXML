
## Should be all element names, since we only want attribute names 
## (alternately we should define to only grab possible attribute names..)
SKIP = c("meta", "children", "member", "row", "cell", "seq")


#' get_level
#' 
#' get a data.frame of attribute values of a given node
#' 
#' @param nex a nexml object
#' @param level a character vector indicating the class of node, see details
#' 
#' @return Returns the attributes of specified class of nodes as a data.frame
#' 
#' @details level should be a character vector giving the path to the specified node
#' group.  For instance, `otus`, `characters`, and `trees` are top-level blocks (e.g.
#' child nodes of the root nexml block), and can be specified directly.  To get metadata
#' for all "char" elements from all characters blocks, you must specify that `char` nodes
#' are child nodes to `character` nodes: e.g. `get_level(nex, "characters/char")`,
#' or similarly for states: `get_level(nex, characters/states)`.  
#' 
#' The return object is a data frame whose columns are the attribute names of the elements
#' specified.  Thus the `id` column refers to the id of the focal node.  Additional columns are
#' added for each parent element in the path; e.g. get_level(nex, "otus/otu") would include a column
#' named "otus" with the id of each otus block.  Even though the method always returns the data frame 
#' for all matching nodes in all blocks, these ids let you see which otu values came from which 
#' otus block.  This is identical to the function call `get_taxa()`.  
#' Similarly, `get_level(nex, "otus/otu/meta")` would return additional columns 'otus' and 
#' also a column, 'otu', with the otu parent ids of each metadata block.  (This is identical to a 
#' function call to `get_metadata`).
#' 
#' @export
#' @importFrom dplyr select
get_level <- function(nex, level){
  lvl <- strsplit(level, "/")[[1]]
  recursion(1, lvl)(nex) %>% dplyr::select_(quote(-nexml))
}


## Trick to apply nodelist_to_df iteratively
closure <- function(level, fun) 
  function(node) nodelist_to_df(node, level, fun)

recursion <- function(i, level){
  if(i < length(level))
    closure(level[i], recursion(i+1, level))
  else
    closure(level[i], attributes_to_row)
}



## Assumes slot(node, element) is a list
#' @importFrom lazyeval interp
#' @importFrom dplyr bind_rows mutate_ %>%
nodelist_to_df <- function(node, element, fn){
  dots <- setNames(list(lazyeval::interp(~x, x = node_id(node))), class(node))
  nodelist <- slot(node, element) 
  if(is.list(nodelist)){ ## node has a list of elements
    nodelist %>% 
      lapply(fn) %>% 
      dplyr::bind_rows() %>%
      dplyr::mutate_(.dots = dots) -> out
  } else { ## handle case when node has only one element
    fn(nodelist)
  }
}

node_id <- function(node){
  if("id" %in% slotNames(node))
    slot(node, "id")
  else
    "root"
}


attributes_to_row <- function(node, skip = SKIP){
  who <- slotNames(node)
  drop <- which(who %in% skip)
  if(length(drop) > 0) who <- who[-drop]
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

