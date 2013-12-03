#' Read NeXML files into various R formats
#' 
#' @param x Path to the file to be read in 
#' @param ... Further arguments passed on to XML::xmlParse
#' @import XML
#' @aliases nexml_read read.nexml 
#' @export nexml_read read.nexml 
#' @examples
#' f <- system.file("examples", "trees.xml", package="RNeXML")
#' nexml_read(f) 
nexml_read <- function(x, ...){
  doc <- xmlParse(x, ...) 
  output <- as(xmlRoot(doc), "nexml")
  free(doc) # explicitly free the pointers after conversion into S4
  output
}


setAs("XMLInternalNode", "phylo", function(from)
   as(as(from, "nexml"), "phylo")
)

read.nexml <- nexml_read


