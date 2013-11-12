#' @export
setGeneric("get_metadata", function(object, level=c("nexml", "otus", "otu", "trees", "tree", "edge", "node")) standardGeneric("get_metadata"))

#' @export
setGeneric("get_taxa", function(object) standardGeneric("get_taxa"))

#' @export
setGeneric("get_tree", function(object) standardGeneric("get_tree"))

#' @export
setGeneric("get_trees", function(object) standardGeneric("get_trees"))


setMethod("get_tree", signature("nexml"), function(object){
          get_item(object, "tree", "phylo")
})


#' Get the desired element from the nexml object
#' @param nexml a nexml object (from read_nexml)
#' @param element 
#' @param level metadata argument only.  Define whose metadata we want. See examples for details.  
#' @export
get_item <- function(nexml, 
                     element = c("tree", "trees", "flat_trees", "metadata", "otu"), 
                     level = c("nexml", "otus", "otu", "trees", "tree")){
  element <- match.arg(element)
  level <- match.arg(level)

  switch(element,
         tree = as(nexml, "phylo"), # will warn if more than one tree is available
         trees = as(nexml, "multiPhylo"),
         flat_trees = flatten_multiphylo(as(nexml, "multiPhylo")),
         metadata = get_metadata(nexml, level),
         otu = get_taxa(nexml))
}



setMethod("get_taxa", 
          signature("nexml"), 
          function(object) 
           sapply(object@otus@otu, function(otu) otu@label)
          )

setMethod("summary", 
          signature("nexml"), 
          function(object, ...) 
            summary(as(object, "phylo"))
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

## FIXME might want to define this for sub-nodes.  e.g. so we can get all metadata on "nodes" in tree2...
## Goodness, but XPATH is so much more expressive for this purpose...
#' get all top-level metadata  More extensible than hardwired functions
setMethod("get_metadata", signature("nexml"), function(object, level){

            level <- match.arg(level) 
            string <- paste0("//nex:", level, "/nex:meta" )
            b <- setxpath(as(object, "XMLInternalElementNode"))

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
          })


#### The following methods are somewhat too rigid.  Might make more sense to do get_metadata(nexml, "nexml")["dc:creator"], etc.  



#' @export
setGeneric("get_license", function(object) standardGeneric("get_license"))
#' @export
setGeneric("get_citation", function(object) standardGeneric("get_citation"))

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







########### nexmlTree class not in active use, lacks a "multiPhylo" concept....


## Would be convenient to inherit these automatically...
setMethod("get_metadata", signature("nexmlTree"), function(object)
          get_metadata(as(object, "nexml")))
setMethod("get_citation", signature("nexmlTree"), function(object)
          get_citation(as(object, "nexml")))
setMethod("get_license", signature("nexmlTree"), function(object)
          get_license(as(object, "nexml")))
setMethod("summary", 
          signature("nexmlTree"), 
          function(object, ...)
            summary(as(object, "nexml"))
          )

