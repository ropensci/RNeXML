
#' @import taxize
#' @export 
taxize_nexml <- function(nexml, type = c("NCBI"), ...){
          type <- match.arg(type)
          if(type == "NCBI"){
            for(i in 1:length(nexml@otus@otu)){
              id <- get_uid(nexml@otus@otu[[i]]@label)
              if(is.na(id))
                warning(paste("ID for otu", nexml@otus@otu[[i]]@label, "not found. Consider checking the spelling or alternate classification"))
              else 
                nexml@otus@otu[[i]]@meta <- new("ListOfmeta", list(
                               meta(href = paste0("http://ncbi.nlm.nih.gov/taxonomy/", id),
                                    rel = "tc:toTaxon")))

            }
          }
  nexml
}
