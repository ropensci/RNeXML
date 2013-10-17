#' @export
setGeneric("get_metadata", function(object) standardGeneric("get_metadata"))

#' @export
setGeneric("get_license", function(object) standardGeneric("get_license"))

#' @export
setGeneric("get_citation", function(object) standardGeneric("get_citation"))


setMethod("summary", 
          signature("nexml"), 
          function(object, ...) 
            summary(as(object, "phylo"))
          )
setMethod("summary", 
          signature("nexmlTree"), 
          function(object, ...)
            summary(as(object, "nexml"))
          )


## Ironically, it is easier to extract the license from the XML representation using XPath than to extract it from the R S4 representation.  



## Using newXMLDoc(object) leads invariably to segfaults....
## safer to write out and parse.  
setxpath <- function(object){
            suppressWarnings(saveXML(object, "tmp.xml"))
            doc <- xmlParse("tmp.xml")
            unlink("tmp.xml")
            doc
}

## Note that we define our namespace prefixes explicitly, so that should the NeXML use a different abberivation, this should still work.  
setMethod("get_citation", 
          signature("nexml"), 
          function(object){
            b <- setxpath(as(object, "XMLInternalElementNode"))
## FIXME should return a citaiton class object! 
            unname(xpathSApply(b, "/nex:nexml/nex:meta[@property='dcterms:bibliographicCitation']/@content", namespaces = nexml_namespaces))
          })

setMethod("get_license",
          signature("nexml"),
          function(object){
            b <- setxpath(as(object, "XMLInternalElementNode"))
            dc_rights <- unname(xpathSApply(b, "/nex:nexml/nex:meta[@property='dc:rights']/@content", namespaces = nexml_namespaces))
            cc_license <- unname(xpathSApply(b, "/nex:nexml/nex:meta[@rel='cc:license']/@href", namespaces = nexml_namespaces))
          if(length(dc_rights) > 0)
            dc_rights
          else
            cc_license
          })

#' get all top-level metadata
setMethod("get_metadata", signature("nexml"), function(object){
            b <- setxpath(as(object, "XMLInternalElementNode"))
            references <- getNodeSet(b, "/nex:nexml/nex:meta[@property]", namespaces = nexml_namespaces)
            rel = sapply(references, 
                              function(x) 
                                xmlAttrs(x)['rel'])
            href = sapply(references, 
                             function(x) 
                               xmlAttrs(x)['href'])
            names(href) = rel
  literals <- getNodeSet(b, "/nex:nexml/nex:meta[@rel]", namespaces = nexml_namespaces)
            property = sapply(literals, 
                              function(x) 
                                xmlAttrs(x)['property'])
            content = sapply(literals, 
                             function(x) 
                               xmlAttrs(x)['content'])
            names(content) = property
            c(content, href)
          })





## Would be convenient to inherit these automatically...
setMethod("get_metadata", signature("nexmlTree"), function(object)
          get_metadata(as(object, "nexml")))
setMethod("get_citation", signature("nexmlTree"), function(object)
          get_citation(as(object, "nexml")))
setMethod("get_license", signature("nexmlTree"), function(object)
          get_license(as(object, "nexml")))

