#' retrieve all available metadata 
#' @param object a representation of the nexml object from  which the data is to be retrieved
#' @param level the level in the nexml for which the metadata is desired
#' @return a named character vector containing all available metadata.  names indicate `property` (or `rel` in the case of links/resourceMeta), while values indicate the `content` (or `href` for links).  
#' @export
#' @seealso  \code{\link{get_item}}
setGeneric("get_metadata", function(object, level=c("nexml", "otus", "otu", "trees", "tree", "edge", "node")) standardGeneric("get_metadata"))

#' Retrieve names of all species/otus otus (operational taxonomic units) included in the nexml 
#' @aliases get_taxa  get_otu
#' @export get_taxa get_otu
#' @seealso  \code{\link{get_item}}
setGeneric("get_otu", function(object) standardGeneric("get_otu"))
setGeneric("get_taxa", function(object) standardGeneric("get_taxa"))

#' extract a phylogenetic tree from the nexml
#' 
#' @param object a representation of the nexml object from  which the data is to be retrieved
#' @return an ape::phylo tree, if only one tree is represented.  Otherwise returns a list of lists of multiphylo trees.  To consistently recieve the list of lists format (preserving the heriarchical nature of the nexml), use \code{\link{get_trees}} instead.  
#' @export
#' @seealso \code{\link{get_trees}} \code{\link{get_flat_trees}} \code{\link{get_item}}
setGeneric("get_tree", function(object) standardGeneric("get_tree"))

#' extract a single multiPhylo object containing all trees in the nexml
#' @details Note that this method collapses any heirachical structure that may have been present as multiple `trees` nodes in the original nexml (though such a feature is rarely used).  To preserve that structure, use \code{\link{get_trees}} instead.  
#' @return a multiPhylo object (list of ape::phylo objects).  See details.  
#' @param object a representation of the nexml object from  which the data is to be retrieved
#' @export
#' @seealso \code{\link{get_tree}} \code{\link{get_trees}} \code{\link{get_item}} 
setGeneric("get_flat_trees", function(object) standardGeneric("get_flat_trees"))
#' extract all phylogenetic trees in ape format
#' 
#' @param object a representation of the nexml object from  which the data is to be retrieved
#' @return returns a list of lists of multiphylo trees, even if all trees are in the same `trees` node (and hence the outer list will be of length 1) or if there is only a single tree (and hence the inner list will also be of length 1.  This guarentees a consistent return type regardless of the number of trees present in the nexml file, and also preserves any heirarchy/grouping of trees.  
#' @export
#' @seealso \code{\link{get_tree}} \code{\link{get_flat_trees}} \code{\link{get_item}}
setGeneric("get_trees", function(object) standardGeneric("get_trees"))


#' Get the desired element from the nexml object
#' @param nexml a nexml object (from read_nexml)
#' @param element the kind of object desired, see details.  
#' @param level metadata argument only.  Define whose metadata we want. See examples for details.  
#' @details
#'  
#' \itemize{
#'  \item{"tree"}{ an ape::phylo tree, if only one tree is represented.  Otherwise returns a list of lists of multiphylo trees.  To consistently recieve the list of lists format (preserving the heriarchical nature of the nexml), use \code{trees} instead.}
#'  \item{"trees"}{ returns a list of lists of multiphylo trees, even if all trees are in the same `trees` node (and hence the outer list will be of length 1) or if there is only a single tree (and hence the inner list will also be of length 1.  This guarentees a consistent return type regardless of the number of trees present in the nexml file, and also preserves any heirarchy/grouping of trees.  }
#'  \item{"flat_trees"}{ a multiPhylo object (list of ape::phylo objects) Note that this method collapses any heirachical structure that may have been present as multiple `trees` nodes in the original nexml (though such a feature is rarely used).  To preserve that structure, use `trees` instead.}
#'  \item{"metadata"}{ }
#'  \item{"otu"}{ returns a named character vector containing all available metadata.  names indicate \code{property} (or \code{rel} in the case of links/resourceMeta), while values indicate the \code{content} (or \code{href} for links). }
#' }
#' For a slightly cleaner interface, each of these elements is also defined as an S4 method
#' for a nexml object.  So in place of `get_item(nexml, "tree")`, one could use `get_tree(nexml)`,
#' and so forth for each element type.  
#' @return return type depends on the element requested.  See details.  
#' @export
#' @seealso \code{\link{get_tree}}
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


# NOTE: See the generics (above) for the documentation of these methods

setMethod("get_tree", signature("nexml"), function(object) get_item(object, "tree"))
setMethod("get_trees", signature("nexml"), function(object) get_item(object, "trees"))
setMethod("get_flat_trees", signature("nexml"), function(object) get_item(object, "flat_trees"))
setMethod("get_otu", signature("nexml"), function(object) get_item(object, "tree"))

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

