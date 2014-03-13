
#' get namespaces
#' 
#' get namespaces
#' @param nexml a nexml object
#' @return a named character vector providing the URLs defining each
#' of the namespaces used in the nexml file.  Names correspond to 
#' the prefix abbreviations of the namespaces. 
#' @export 
get_namespaces <- function(nexml){
  nexml@namespaces
}
