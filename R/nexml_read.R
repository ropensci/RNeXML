
#' Read NeXML files into various R formats
#' 
#' @param x Path to the file to be read in 
#' @param type the type of object to be returned.  If the file 
#' contains multiple trees, all will be read into the appropriate 
#' multi-tree container, or else returned as a list of such objects.  
#' @aliases read.nexml 
#' @import XML
#' @import ape 
#' @export
#' @examples
#' f <- system.file("examples", "trees.xml", package="RNeXML")
#' nexml_read(f) 
nexml_read <- function(x, type = c("phylo", "phylo4", "ouch",
                                   "matrix", "nexml")){
  type <- match.arg(type) 
  doc <- xmlParse(x) 
  # will return class multiphylo if phylo is asked for and multiple trees are given.  
  as(doc, type)
}


# possibly not a good conversion to define
setAs("XMLInternalDocument", "phylo", function(from)
   as(as(xmlRoot(doc), "nexml"), "phylo")
)



