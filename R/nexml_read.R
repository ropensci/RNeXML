#' Read NeXML files into various R formats
#' 
#' @param x Path to the file to be read in. Or an XMLInternalDoc or XMLInternalNode 
#' @param ... Further arguments passed on to XML::xmlParse
#' @import XML
#' @import httr
#' @aliases nexml_read read.nexml 
#' @export nexml_read read.nexml 
#' @examples
#' f <- system.file("examples", "trees.xml", package="RNeXML")
#' nexml_read(f) 
nexml_read <- function(x, ...){
  if(length(grep("^https?://", x)) > 0){ # handle remote paths using httr::GET
    x <- GET(x)
    doc <- xmlParse(x, ...) 
    output <- as(xmlRoot(doc), "nexml")
    free(doc) # explicitly free the pointers after conversion into S4
  } else if(file.exists(x)){
    doc <- xmlParse(x, ...) 
    output <- as(xmlRoot(doc), "nexml")
    free(doc) 
  } else if(is(x, "XMLInternalDocument")){
    output <- as(xmlRoot(doc), "nexml")
  } else if(is(x, "XMLInternalNode")){
    output <- as(x, "nexml")
  }

  output
}


setAs("XMLInternalNode", "phylo", function(from)
   as(as(from, "nexml"), "phylo")
)

read.nexml <- nexml_read


