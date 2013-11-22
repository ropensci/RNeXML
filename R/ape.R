## These are really more like specific functions rather than generic methods .... 
setGeneric("toPhylo", function(tree, otus) standardGeneric("toPhylo"))
setGeneric("getTaxonNames", function(otus, ids) standardGeneric("getTaxonNames"))

#' @import plyr
#' @import ape 
# Developer Note: This function simply calls `toPhylo` in the appropriate way 
# depending on the approriate number of trees in the nexml.  



# 
setAs("nexml", "multiPhyloList", function(from){
   map <- get_otu_maps(from) 
   unname(lapply(from@trees, 
           function(X){
             out <- unname(lapply(X@tree,  toPhylo, map[[X@otus]]))
             class(out) <- "multiPhylo"
             out
           }))
})


# Always collapses all trees nodes into a multiphylo
setAs("nexml", "multiPhylo", function(from){
   map <- get_otu_maps(from) 
   out <- unname(lapply(from@trees, 
           function(X){
             out <- unname(lapply(X@tree,  toPhylo, map[[X@otus]]))
             class(out) <- "multiPhylo"
             out
           }))
  flatten_multiphylo(out)
})


#' Flatten a multiphylo object
#' 
#' @details NeXML has the concept of multiple <trees> nodes, each with multiple child <tree> nodes.
#' This maps naturally to a list of multiphylo  objects.  Sometimes
#' this heirarchy conveys important structural information, so it is not discarded by default. 
#' Occassionally it is useful to flatten the structure though, hence this function.  Note that this
#' discards the original structure, and the nexml file must be parsed again to recover it.  
#' @param object a list of multiphylo objects 
#' @export
flatten_multiphylo <- function(object){
  out <- unlist(object, FALSE, FALSE)
  class(out) <- "multiPhylo"
  out
}


setAs("nexml", "phylo", function(from){ 
    if(length(from@trees[[1]]@tree) == 1){
      maps <- get_otu_maps(from)
      otus_id <- from@trees[[1]]@otus
      out <- toPhylo(from@trees[[1]]@tree[[1]], maps[[otus_id]])
    } else { 
      warning("Multiple trees found, Returning multiPhylo object")
      out <- as(from, "multiPhylo") 
    }
    if(length(out) == 1)
     out <- flatten_multiphylo(out)
   out 
  })






setAs("phylo", "tree", function(from){
  phy <- from
  ## Generate the "ListOfedge" made of "edge" objects

  edges <- 
    lapply(1:dim(phy$edge)[1], 
           function(i){
            e <- new("edge", 
                source = paste("n",phy$edge[i,1], sep=""), 
                target = paste("n",phy$edge[i,2], sep=""), 
                id = paste("e", i, sep=""))
           if(!is.null(phy$edge.length))
             e@length <- as.numeric(phy$edge.length[i])
           e
           }
  )
  edges <- new("ListOfedge", edges)
  ## Generate the ListOfnode made of "node" objects
  nodes <- lapply(unique(as.numeric(phy$edge)), function(i){
    if(is.na(phy$tip.label[i]))
      new("node", id = paste("n", i, sep=""))
    else if(is.character(phy$tip.label[i]))
      new("node", id = paste("n", i, sep=""), otu = paste0("t", i))  #phy$tip.label[i])  ## OTUs get abstract names
  })
  nodes <- new("ListOfnode", nodes)

  ## Create the "tree" S4 object
  new("tree", 
      node = nodes, 
      edge = edges,
      'xsi:type' = 'nex:FloatTree',
      id = "tree1")  # Okay if we update ids later to avoid clashes?  
  ##  UUIDs create errors: is not a valid value of the atomic type 'xs:ID'
})


setMethod("toPhylo",
          signature("tree", "character"),
          function(tree, otus){

## Consider for loops instead here
        missing_as_na <- function(x){
          if(length(x) == 0)
            NA
          else
            unname(x)
        }

        nodes <- sapply(unname(tree@node), function(x) c(node = unname(x@id), otu = missing_as_na(x@otu)))

        
        # If any edges have lengths
        if(any(sapply(tree@edge, function(x) length(x@length) > 0)))
          edges <- sapply(unname(tree@edge), function(x) c(source = unname(x@source), 
                                                          target = unname(x@target), 
                                                          length = if(identical(x@length, numeric(0))) NA else unname(x@length), 
                                                          id = unname(x@id)))
        else # no edge lengths 
          edges <- sapply(unname(tree@edge), function(x) c(source = unname(x@source), 
                                                          target = unname(x@target), 
                                                          id = unname(x@id)))

        nodes <- data.frame(t(nodes), stringsAsFactors=FALSE)
        names(nodes) <- c("node", "otu")

## Identifies tip.label based on being named with OTUs while others are NULL
## Should instead decide that these are tips based on the edge labels?
        nodes <- cbind(plyr::arrange(nodes, otu), id = 1:dim(nodes)[1])


##  nodes$node lists tip taxa first.  APE expects nodes numbered 1:n_tips to be
## to correspond to tips... 
        source_nodes <- match(edges["source",], nodes$node)
        target_nodes <- match(edges["target",], nodes$node)

##      Define elements of a phylo class object
        edge <- unname(cbind(source_nodes, target_nodes))
        if("length" %in% rownames(edges))
          edge.length <- as.numeric(edges["length",])       
        else
          edge.length <- NULL

        tip_otus <- as.character(na.omit(nodes$otu))   
        tip.label <- otus[tip_otus]

        Nnode <- length(tip.label) - 1 
        phy = list(edge=edge, 
                   tip.label = unname(tip.label), 
                   Nnode = Nnode)
        if(!is.null(edge.length))
          phy$edge.length = edge.length # optional fields
        class(phy) = "phylo"
        phy
      }
)




setMethod("getTaxonNames",
          signature("otus", "character"),
          function(otus, ids){
            taxon <- sapply(otus@otu, 
                            function(x){
                              if(length(otu@label) > 0) 
                                label <- otu@label
                              else
                                label <- otu@id
                              c(otu@id, label)
                            })
            out <- taxon[2, ] 
            names(out) <- taxon[1, ]
            out[ids]
          })


##################### phylo -> nexml #################################3


setAs("phylo", "nexml", function(from){
  trees = new("trees", tree=new("ListOftree", list( as(from, "tree") )))
  otus = as(from, "otus")
  otus@id = "tax1" #UUIDgenerate()
  trees@id = "Trees" #UUIDgenerate()
  trees@otus = otus@id
  new("nexml", 
      trees = new("ListOftrees", list(trees)),
      otus = new("ListOfotus",list(otus)))
})

setAs("phylo", "otus", function(from){
  otu_list <- lapply(1:length(from$tip.label), 
                     function(i)
                       new("otu", id = paste0("t",i), label = from$tip.label[i]) )
  new("otus", otu=new("ListOfotu", otu_list))
})




## Convience coercions 


setAs("multiPhylo", "trees", function(from)
  new("trees", tree = from))

setAs("multiPhylo", "nexml", function(from)
      as(as(from, "trees"), "nexml"))



#' get otus map
#'
#' get a list showing the mapping between (internal) otu identifiers and labels (taxonomic names). List is named by the id of the otus block. If no labels are found, just return the ids in place of labels.   
#' @param nexml nexml object 
#' @return a list showing the mapping between (internal) otu identifiers and labels (taxonomic names). List is named by the id of the otus block. 
#' @details largely for internal use   
#' @export 
get_otu_maps <- function(nexml){
  otus <- nexml@otus
  ids <- sapply(otus, function(otus) otus@id)
  names(otus) <- ids
  otu_maps <- 
    lapply(otus, function(otus){ # loop over all otus nodes  
    # getTaxonNames(otus) ## Equivalent to the below...
    taxon <- sapply(otus@otu, function(otu){ # loop over each otu in the otus set
      if(length(otu@label) > 0) 
        label <- otu@label
      else
        label <- otu@id
      c(otu@id, label)
    })
    out <- taxon[2, ] #label 
    names(out) <- taxon[1, ] #id 
    out
  })
  otu_maps
}



