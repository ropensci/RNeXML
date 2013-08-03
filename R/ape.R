
setAs("phylo", "tree", function(from){
  phy <- from

  ## Generate the "ListOfedge" made of "edge" objects
  edges <- lapply(1:dim(phy$edge)[1], function(i)
    new("edge", 
        source = paste("n",phy$edge[i,1], sep=""), 
        target = paste("n",phy$edge[i,2], sep=""), 
        length = as.numeric(phy$edge.length[i]), # Add this only if provided! shouldn't error on null edge lengths
        id = paste("e", i, sep=""))
  )
  edges <- new("ListOfedge", edges)

  ## Generate the ListOfnode made of "node" objects
  nodes <- lapply(unique(as.numeric(phy$edge)), function(i)
    new("node", id = paste("n", i, sep=""), otu = phy$tip.labels[i])
  )
  nodes <- new("ListOfnode", nodes)

  ## Create the "tree" S4 object
  new("tree", nodes = nodes, edges = edges)
})


#' @import plyr
#' @import XML
setAs("tree", "phylo", 
      function(from){

        tree <- from
## Consider for loops instead here
        missing_as_na <- function(x){
          if(length(x) == 0)
            NA
          else
            unname(x)
        }

        nodes <- sapply(unname(tree@nodes), function(x) c(node = unname(x@id), otu = missing_as_na(x@otu)))
        edges <- sapply(unname(tree@edges), function(x) c(source = unname(x@source), 
                                                          target = unname(x@target), 
                                                          length = x@length, 
                                                          id = unname(x@id)))


        nodes <- data.frame(t(nodes), stringsAsFactors=FALSE)
        names(nodes) <- c("node", "label")

## Identifies tip.labels based on being named with OTUs while others are NULL
## Should instead decide that these are tips based on the edge labels?
        nodes <- cbind(arrange(nodes, label), id = 1:dim(nodes)[1])


##  nodes$node lists tip taxa first.  APE expects nodes numbered 1:n_tips to be
## to correspond to tips... 
        source_nodes = match(edges["source",], nodes$node)
        target_nodes = match(edges["target",], nodes$node)

##      Define elements of a phylo class object
        edge = unname(cbind(source_nodes, target_nodes))
        tip.labels = as.character(na.omit(nodes$label))
        Nnode = length(tip.labels) - 1 
        edge.length = as.numeric(edges["length",])



        phy = list(edge=edge, tip.labels = tip.labels, Nnode = Nnode, # required fields
                    edge.length = edge.length # optional fields
                    )
        class(phy) = "phylo"
        phy
      }
)


# Method to go directly from XML to phylo via S4 "tree"
setAs("XMLInternalElementNode", "phylo", function(from){
      as(as(from, "tree"), "phylo")
})



