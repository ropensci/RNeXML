
#' @import plyr
#' @import ape 


## nexml_to_phylo 
# @param tree an nexml tree element (nexml@trees[[i]]@tree[[j]]
# @param otus a character string of taxonomic labels, named by the otu ids.  
# e.g. (from get_otu_maps for the otus set matching the relevant trees node. 
# @return phylo object 
toPhylo <- function(tree, otus){

  ## Extract the nodes list
  nodes <- sapply(unname(tree@node), 
                  function(x) 
                    c(node = unname(x@id), 
                      otu = missing_as_na(x@otu)))
  
  # If any edges have lengths, use this routine
  if(any(sapply(tree@edge, function(x) length(x@length) > 0)))
    edges <- sapply(unname(tree@edge), 
                    function(x) 
                      c(source = unname(x@source), 
                        target = unname(x@target), 
                        length = if(identical(x@length, numeric(0)))
                                  NA 
                                 else 
                                   unname(x@length), 
                        id = unname(x@id)))
  else # no edge lengths, use this routine 
    edges <- sapply(unname(tree@edge), 
                    function(x) 
                      c(source = unname(x@source), 
                        target = unname(x@target), 
                        id = unname(x@id)))

  nodes <- data.frame(t(nodes), stringsAsFactors=FALSE)
  names(nodes) <- c("node", "otu")

## Identifies tip.label based on being named with OTUs while others are NULL
## FIXME Should instead decide that these are tips based on the edge labels?
  nodes <- cbind(plyr::arrange(nodes, otu), id = 1:dim(nodes)[1])
## NB: these ids are the ape:id numbers by which nodes are identified in ape::phylo
## Arbitrary ids are not supported - ape expecs the numbers 1:n, starting with tips. 

##  nodes$node lists tip taxa first (see arrange fn above), since
##  APE expects nodes numbered 1:n_tips to be to correspond to tips.
  source_nodes <- match(edges["source",], nodes$node)
  target_nodes <- match(edges["target",], nodes$node)

  ##  Define elements of a phylo class object ##
  #--------------------------------------------#

  ## define edge matrix
  edge <- unname(cbind(source_nodes, target_nodes))
  if("length" %in% rownames(edges))
    edge.length <- as.numeric(edges["length",])       
  else
    edge.length <- NULL

  ## define tip labels
  tip_otus <- as.character(na.omit(nodes$otu))   
  tip.label <- otus[tip_otus]

  # Count internal nodes (assumes bifurcating tree. Does ape always assume this?) 
  # FIXME use a method that does not assume bifurcating tree... 
  Nnode <- length(tip.label) - 1 

  # assemble the phylo object, assign class and return.  
  phy = list(edge=edge, 
             tip.label = unname(tip.label), 
             Nnode = Nnode)
  if(!is.null(edge.length))
    phy$edge.length = edge.length # optional fields
  class(phy) = "phylo"
  phy
}


## Helper function
missing_as_na <- function(x){
  if(length(x) == 0)
    NA
  else
    unname(x)
}






