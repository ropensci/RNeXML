
## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...

#' get_metadata
#' 
#' get_metadata 
#' @param nexml a nexml object
#' @param level the name of the level of element desired, or
#'  use "all" to access all meta elements.  
#' @return the requested metadata
#' @import XML
#' @examples
#' comp_analysis <- system.file("examples", "comp_analysis.xml", package="RNeXML")
#' nex <- nexml_read(comp_analysis)
#' get_metadata(nex)
#' @export
get_metadata <- function(nex, level = "nexml", depth = 1){
  
  # level = c("nexml", "otus", "otu", "trees", "tree", "characters", "format", "states", "all")
  nodes <- 
    switch(level, 
           nexml = nex,
           otus = slot(nex, "otus")[[depth]],
           otu = slot(slot(nex, "otus")[[depth]], "otu"),
           trees = slot(nex, "trees")[[depth]],
           tree = slot(slot(nex, "trees")[[depth]], "tree"),
           characters = slot(nex, "characters")[[depth]],
           format = slot(nex, "format")[[depth]],
           states = slot(nex, "states")[[depth]])
  
  if(is(nodes, "list"))
    out <- dplyr::bind_rows(lapply(nodes, function(x){
      nexlist_as_data_frame(slot(x, "meta"), append = parent_id(x))
    }))
  else
    out <- nexlist_as_data_frame(slot(nodes, "meta"), append = parent_id(nodes))
  
  out
}

parent_id <- function(node){
  if("id" %in% slotNames(node))
    slot(node, "id")[[1]]
  else
    NULL
}


## Assumes meta elements do not have child meta elements
nexlist_as_data_frame <- function(nexlist, append = NULL, skip = c("meta", "children")){
  if(length(nexlist) > 0){ 
    out <- dplyr::bind_rows(lapply(nexlist, function(node){
      who <- slotNames(node)
      who <- who[-which(who %in% skip)]
      tmp <- sapply(who, function(x) slot(node, x))
      tmp[sapply(tmp,length) < 1] <- NA
      data.frame(as.list(tmp), stringsAsFactors=FALSE)
    }))
    if(!is.null(append)) out <- data.frame(out, parent_id = append, stringsAsFactors = FALSE)
  out
  } else { 
    NULL
  }
}




## Depricated

get_metadata2 <-  function(nexml, level="nexml"){
  # c("nexml", "otus", "otu", "trees", "tree", "characters", "format", "states", "all")
  #  level <- match.arg(level)  # Don't insist a match for more flexibilty.  
  string <- paste0("//", level, "/meta" )
  if(level == "all")
    string <- paste0("//meta")
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
