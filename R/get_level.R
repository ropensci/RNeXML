
## Should be all element names, since we only want attribute names 
## (alternately we should define to only grab possible attribute names..)
SKIP = c("meta", "children", "member", "row", "cell")


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

