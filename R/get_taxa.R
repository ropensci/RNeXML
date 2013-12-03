
#' Retrieve names of all species/otus otus (operational taxonomic units) included in the nexml 
#' @aliases get_taxa  get_otu
#' @export get_taxa get_otu
#' @seealso  \code{\link{get_item}}
get_taxa <-  
          function(nexml){
            out <- lapply(nexml@otus, function(otus)
              sapply(otus@otu, function(otu) otu@label))
            unname(unlist(out, recursive = FALSE))
          }
get_otu <- get_taxa

#' Retrieve names of all species/otus otus (operational taxonomic units) included in the nexml 
#' @aliases get_taxa_list get_otus_list
#' @export get_taxa_list get_otus_list
#' @seealso  \code{\link{get_item}}
get_taxa_list <-
          function(nexml){
            out <- lapply(nexml@otus, function(otus){
              out <- sapply(otus@otu, function(otu) otu@label)
              names(out) <- name_by_id(otus@otu)
              out
              })
            names(out) <- name_by_id(nexml@otus)
            out
          }
get_otus_list <- get_taxa_list


