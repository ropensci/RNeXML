#' Add metadata to a nexml file
#' 
#' @param meta a meta S4 object, e.g. ouput of the function \code{\link{meta}}, or a list of these meta objects
#' @param a nexml (S4) object
#' @param level the level at which the metadata annotation should be added. 
#' @param i for otus, trees, characters: if there are multiple such blocks, which one should be annotated?  Default is first/only block.  
#' @param at_id the id of the element to be annotated.  Optional, advanced use only.  
#' @seealso \code{\link{add_trees}} \code{\link{add_characters}}
#' @return the updated nexml object
#' @examples
#' modified <- meta(property = "prism:modificationDate",
#'                 content = "2013-10-04")
#'  add_meta(modified)
#' @export add_meta
#' 
add_meta <- function(meta, nexml=new("nexml"), level=c("nexml", "otus", "trees", "characters"), i = 1, at_id = NULL){
  level <- match.arg(level)
  if(is(meta, "meta"))
    meta <- list(meta)
  if(!all(sapply(meta, is, "meta")))
    stop("All elements in list must be of class 'meta'")
 
  if(!is.null(at_id)){
    stop("function does not yet handle at_id assignments")
    # case not written yet
  } else if(level =="nexml"){ 
    nexml@meta <- new("ListOfmeta", c(nexml@meta, meta))
  } else if(level =="otus"){ 
    nexml@otus[[i]]@meta <- new("ListOfmeta", c(nexml@otus[[i]]@meta, meta))
  }  else if(level =="nexml"){ 
    nexml@trees[[i]]@meta <- new("ListOfmeta", c(nexml@trees[[i]]@meta, meta))
  }  else if(level =="nexml"){ 
    nexml@characters[[i]]@meta <- new("ListOfmeta", c(nexml@characters[[i]]@meta, meta))
  } 


  nexml

}

