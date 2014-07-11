
## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...

#' get_metadata
#' 
#' get_metadata 
#' @param nexml a nexml object
#' @param level the name of the level of element desired, or
#'  use "all" to access all meta elements.  
#' @return the requested metadata
#' @import XML
#' @export
get_metadata <-  function(nexml, level="nexml"){
# c("nexml", "otus", "otu", "trees", "tree", "characters", "format", "states", "all")
  #  level <- match.arg(level)  # Don't insist a match for more flexibilty.  
  string <- paste0("//nex:", level, "/nex:meta" )
  if(level == "all")
    string <- paste0("//nex:meta")
  b <- setxpath(as(nexml, "XMLInternalElementNode"))

  references <- getNodeSet(b, 
                           paste0(string, "[@rel]"),
                           namespaces = nexml_namespaces)
  rel = sapply(references, 
                    function(x) 
                      xmlAttrs(x)['rel'])
  href = sapply(references, 
                   function(x) 
                     xmlAttrs(x)['href'])
  names(href) = rel
  literals <- getNodeSet(b, 
                         paste0(string, "[@property]"), 
                         namespaces = nexml_namespaces)
  property = sapply(literals, 
                    function(x) 
                      xmlAttrs(x)['property'])
  content = sapply(literals, 
                   function(x) 
                     xmlAttrs(x)['content'])
  names(content) = property
  c(content, href)
}

## Ironically, it is easier to extract the license from the XML representation using XPath than to extract it from the R S4 representation.  
## Using newXMLDoc(object) leads invariably to segfaults....
## safer to write out and parse.  
setxpath <- function(object){
            tmp <- tempfile()
            suppressWarnings(saveXML(object, tmp))
            doc <- xmlParse(tmp)
            unlink(tmp)
            doc
}


