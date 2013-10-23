#' Read NeXML files into various R formats
#' 
#' @param x Path to the file to be read in 
#' @param type the type of object to be returned.  If the file 
#' contains multiple trees, all will be read into the appropriate 
#' multi-tree container, or else returned as a list of such objects.  
#' @import XML
#' @import ape 
#' @aliases nexml_read read.nexml 
#' @export nexml_read read.nexml 
#' @examples
#' f <- system.file("examples", "trees.xml", package="RNeXML")
#' nexml_read(f) 
nexml_read <- function(x, type = c("nexmlTree", "phylo", "phylo4", "ouch",
                                   "matrix", "nexml")){
  type <- match.arg(type) 
  doc <- xmlParse(x) 
  # will return class multiphylo if phylo is asked for and multiple trees are given.  
  output <- as(xmlRoot(doc), type)
  free(doc)
  output
}


setAs("XMLInternalNode", "phylo", function(from)
   as(as(from, "nexml"), "phylo")
)

read.nexml <- nexml_read


