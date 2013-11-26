#### from nexml


setAs("nexml", "multiPhyloList", function(from){
   map <- get_otu_maps(from) 
   unname(lapply(from@trees, 
           function(X){
             out <- unname(lapply(X@tree,  toPhylo, map[[X@otus]]))
             class(out) <- "multiPhylo"
             out
           }))
})


# Always collapses all trees nodes into a multiphylo
setAs("nexml", "multiPhylo", function(from){
   map <- get_otu_maps(from) 
   out <- unname(lapply(from@trees, 
           function(X){
             out <- unname(lapply(X@tree,  toPhylo, map[[X@otus]]))
             class(out) <- "multiPhylo"
             out
           }))
  flatten_multiphylo(out)
})


#' Flatten a multiphylo object
#' 
#' @details NeXML has the concept of multiple <trees> nodes, each with multiple child <tree> nodes.
#' This maps naturally to a list of multiphylo  objects.  Sometimes
#' this heirarchy conveys important structural information, so it is not discarded by default. 
#' Occassionally it is useful to flatten the structure though, hence this function.  Note that this
#' discards the original structure, and the nexml file must be parsed again to recover it.  
#' @param object a list of multiphylo objects 
#' @export
flatten_multiphylo <- function(object){
  out <- unlist(object, FALSE, FALSE)
  class(out) <- "multiPhylo"
  out
}


setAs("nexml", "phylo", function(from){ 
    if(length(from@trees[[1]]@tree) == 1){
      maps <- get_otu_maps(from)
      otus_id <- from@trees[[1]]@otus
      out <- toPhylo(from@trees[[1]]@tree[[1]], maps[[otus_id]])
    } else { 
      warning("Multiple trees found, Returning multiPhylo object")
      out <- as(from, "multiPhylo") 
    }
    if(length(out) == 1)
     out <- flatten_multiphylo(out)
   out 
  })



