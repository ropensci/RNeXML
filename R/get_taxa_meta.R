### TODO: how to deal with missing meta slot elements???
### Right now the functions break when an otu doesn't have meta slot


#' get_taxa_meta
#'
#' Retrieve metadata of all species/otus otus (operational taxonomic units) included in the nexml
#' @aliases get_taxa_meta get_otu_meta
#' @param nexml a nexml object
#' @param what One of href, rel, id, or xsi:type
#' @return the list of metadata for each taxon
#' @export get_otu_meta get_taxa_meta
#' @seealso  \code{\link{get_item}}
#' @examples 
#' data(bird.orders)
#' birds <- add_trees(bird.orders)
#' birds <- taxize_nexml(birds, "NCBI")
#' get_taxa_meta(birds)
#' get_taxa_meta(birds, 'rel')
#' get_taxa_meta(birds, 'id')
#' get_taxa_meta(birds, 'xsi:type')
get_taxa_meta <-  
  function(nexml, what='href'){
    out <- lapply(nexml@otus, function(otus)
#       sapply(otus@otu, function(otu) slot(otu@meta, what)))
    sapply(otus@otu, function(otu) slot(otu@meta[[1]], what)))
    unname(unlist(out, recursive = FALSE))
  }
get_otu_meta <- get_taxa_meta

#' get_taxa_meta_list
#'
#' Retrieve metadata of all species/otus otus (operational taxonomic units) included in the nexml
#' @aliases get_taxa_meta_list get_otu_meta_list
#' @param nexml a nexml object
#' @param what One of href, rel, id, or xsi:type
#' @return the list of metadata for each taxon
#' @seealso  \code{\link{get_item}}
#' @export get_otu_meta_list get_taxa_meta_list
#' @examples
#' data(bird.orders)
#' birds <- add_trees(bird.orders)
#' birds <- taxize_nexml(birds, "NCBI")
#' get_taxa_meta_list(birds)
#' get_taxa_meta_list(birds, 'rel')
#' get_taxa_meta_list(birds, 'id')
#' get_taxa_meta_list(birds, 'xsi:type')
get_taxa_meta_list <-
  function(nexml, what='href'){
    out <- lapply(nexml@otus, function(otus){
      out <- sapply(otus@otu, function(otu) slot(otu@meta[[1]], what))
      names(out) <- name_by_id(otus@otu)
      out
    })
    names(out) <- name_by_id(nexml@otus)
    out
  }
get_otu_meta_list <- get_taxa_meta_list