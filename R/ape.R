## These are really more like specific functions rather than generic methods .... 
setGeneric("toPhylo", function(tree, otus) standardGeneric("toPhylo"))
setGeneric("getTaxonNames", function(otus, ids) standardGeneric("getTaxonNames"))

#' @import plyr
#' @import ape 
# Developer Note: This function simply calls `toPhylo` in the appropriate way 
# depending on the approriate number of trees in the nexml.  
setAs("nexml", "phylo", function(from){ 
  # If there are mutiple trees nodes, return list of multiphylo with warning
  if(length(from@trees) > 1){ 
    warning("Returning a list of multiPhylo objects")
    lapply(from@trees, 
           function(X){
            out <- lapply(X@tree,  toPhylo, from@otus)
            class(out) <- "multiPhylo"
            out 
           })

  } else if(length(from@trees) == 1){
    # If there are multiple tree nodes in a trees node, return a multiphylo with warning
    if(length(from@trees[[1]]@tree) > 1){
      warning("Returning multiple trees as a multiPhylo object")
      out <- lapply(from@trees[[1]]@tree,  toPhylo, from@otus)
      class(out) <- "multiPhylo"
      out

  # If there is one tree node, return "phylo"
    } else if(length(from@trees[[1]]@tree) == 1){
      toPhylo(from@trees[[1]]@tree[[1]], from@otus)
    }
  }
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


setMethod("getTaxonNames",
          signature("otus", "character"),
          function(otus, ids){
            taxon <- sapply(otus@otu, 
                            function(x){
                              c(x@id, x@label)
                            })
            out <- taxon[2, ] # FIXME better as taxon["label",] ?  what about if label not given?  
            names(out) <- taxon[1, ]
            out[ids]
          })

setMethod("toPhylo",
          signature("tree", "otus"),
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
        tip.label <- getTaxonNames(otus, tip_otus)

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



##################### phylo -> nexml #################################3


setAs("phylo", "nexml", function(from){
  trees = new("trees", tree=new("ListOftree", list( as(from, "tree") )))
  otus = as(from, "otus")
  otus@id = "tax1" #UUIDgenerate()
  trees@id = "Trees" #UUIDgenerate()
  trees@otus = otus@id
  new("nexml", 
      trees = new("ListOftrees", list(trees)),
      otus = otus)
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


