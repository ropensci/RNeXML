
#' Concatenate nexml files 
#' @param a nexml object, e.g. from write.nexml() or read.nexml()
#' @param ... additional nexml objects to be concatenated.  
#'   must have unique ids on all elements
#' @return a concatenated nexml file
#' @examples
#' f1 <- system.file("examples", "trees.xml", package="RNeXML")
#' f2 <- system.file("examples", "comp_analysis.xml", package="RNeXML")
#' nex1 <- read.nexml(f1)
#' nex2 <- read.nexml(f2)
#' nex <- c(nex1, nex2)
setMethod("c", 
          signature("nexml"), 
          function(x, ...){
              elements = list(x, ...)
              nexml <- new("nexml")
  ## Check that ids are unique
  if(!do.call(unique_ids,elements))
    stop("ids are not unique across nexml files. 
          Consider regenerating ids")
  else {

  nexml@otus <- new("ListOfotus", 
                    unlist(lapply(elements, 
                                  function(n) n@otus), 
                           recursive=FALSE))
  nexml@characters <- new("ListOfcharacters", 
                    unlist(lapply(elements, 
                                  function(n) n@characters), 
                           recursive=FALSE))
  nexml@trees <- new("ListOftrees", 
                    unlist(lapply(elements, 
                                  function(n) n@trees), 
                           recursive=FALSE))
  }
  nexml
})

get_ids <- function(nexml){
  doc <- xmlDoc(as(nexml, "XMLInternalNode"))
  out <- unname(xpathSApply(doc, "//@id"))
  free(doc)
  out
}

unique_ids <- function(...){
  set <- list(...)
  counts <- table(unlist(lapply(set, get_ids)))
  !any(counts > 1)
}

