
#' @import taxize
setGeneric("addIdentifiers", function(object, type = c("NCBI"), ...) standardGeneric("addIdentifiers"))
setMethod("addIdentifiers", signature("nexml"), function(object, type, ...){
          type <- match.arg(type)
          if(type == "NCBI"){
            for(i in 1:length(object@otus@otu)){
              id <- get_uid(object@otus@otu[[i]]@label)
              if(is.na(id))
                warning(paste("ID for otu", object@otus@otu[[i]]@label, "not found. Consider checking the spelling or alternate classification"))
              else 
                object@otus@otu[[i]]@meta <- new("ListOfmeta", list(
                               meta(href = paste0("http://ncbi.nlm.nih.gov/taxonomy/", id),
                                    rel = "tc:toTaxon")))

            }
          }
  object
})
