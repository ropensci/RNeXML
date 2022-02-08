
#' taxize nexml
#'
#' Check taxonomic names against the specified service and 
#' add appropriate semantic metadata to the nexml OTU unit 
#' containing the corresponding identifier. 
#' @param nexml a nexml object
#' @param type the name of the identifier to use
#' @param warnings should we show warning messages if no match can be found?
#' @param ... additional arguments to `[taxadb::get_ids()]`
#' @export 
#' @examples \dontrun{
#' data(bird.orders)
#' birds <- add_trees(bird.orders)
#' birds <- taxize_nexml(birds, "NCBI")
#' }
# @importFrom taxize get_uid
taxize_nexml <- function(nexml, 
                         type = c("ncbi", "itis", "col", "tpl",
                                  "gbif", "wd"), 
                         warnings = TRUE,
                         ...){
  
  ## Soft dependency on taxadb
  if (!requireNamespace("taxalight", quietly = TRUE)) {
    stop("taxadb package required to convert look up taxonomic ids",
         call. = FALSE)
  }
  get_ids <- getExportedValue("taxalight", "get_ids")
  
  type <- tolower(type)
  type <- match.arg(type)

  for(j in 1:length(nexml@otus)){
    
    # Resolve all ids at once
    labels <- unname(vapply(nexml@otus[[j]]@otu, slot, character(1L), "label"))
    clean_labels <- gsub("_", " ", labels)
    taxa_ids <- get_ids(clean_labels, type, ...)
    
    for(i in 1:length(taxa_ids)){
      id <- taxa_ids[[i]]
      if(is.na(id) & warnings)
        warning(paste("ID for otu", 
                      nexml@otus[[j]]@otu[[i]]@label, 
                      "not found. Consider checking the spelling
                      or alternate classification"))
      else 
        nexml@otus[[j]]@otu[[i]]@meta <- 
          c(nexml@otus[[j]]@otu[[i]]@meta, 
                       meta(href = taxa_ids[[i]],
                            rel = "tc:toTaxon"))

    }
  }
        
  nexml
}
