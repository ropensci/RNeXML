
##################### phylo -> nexml ###############

setAs("phylo", "nexml", function(from){
      add_trees(from)
})



#' @export 
add_trees <- function(phy, nexml=new("nexml")){
  nexml <- as(nexml, "nexml")

  ## handle multiphlyo cases
  if(is(phy, "list") && 
     is(phy[[1]], "list") && 
     is(phy[[1]][[1]], "phylo"))
    new_taxa <- unlist(sapply(x, function(y)
                              sapply(y, function(z) 
                                     z$tip.label)))
  else if(is(phy, "multiPhylo"))    
    new_taxa <- unlist(sapply(x, function(y) y$tip.label))
  else if(is(phy, "phylo"))
    new_taxa <- phy$tip.label


  nexml <- add_otu(nexml, new_taxa)
  otus_id <- nexml@otus[[length(nexml@otus)]]@id
  nexml <- add_trees_block(nexml, phy, otus_id)
  nexml
}

add_trees_block <- function(nexml, phy, otus_id){
  if(is(phy, "list") && is(phy[[1]], "list") && is(phy[[1]][[1]], "phylo"))
   n_trees <- length(phy) 
  else if(is(phy, "multiPhylo"))  
    phy <- list(phy) # make a list of multiphylo 
  else if(is(phy, "phylo")){
    phy <- list(phy)
    class(phy) <- "multiPhylo"
    phy <- list(phy)
  } else
    stop("class of object phy not recognized as an ape::phylo, ape::multiPhylo, or list of ape::multiPhylo objects")

  ## all trees will use the same  
  otu_map <- reverse_map(get_otu_maps(nexml))[[otus_id]]

  trees <- lapply(phy, function(trs){
         tree_id <- nexml_id("ts")
         new("trees", 
             id = tree_id,
             about = paste0("#", tree_id),
             otus = otus_id,
             tree = new("ListOftree", 
                        lapply(trs, function(tr)
                           fromPhylo(tr, otu_map)))
            )
  })

  ## Append to any existing trees nodes 
  nexml@trees <- new("ListOftrees", c(nexml@trees, trees))
  nexml
}



fromPhylo <- function(phy, otu_map){
  
  node_ids <- sapply(unique(as.numeric(phy$edge)), 
                     function(i) nexml_id("n")) 
  names(node_ids) <- as.character(unique(as.numeric(phy$edge)))

  ## Generate the "ListOfedge" made of "edge" objects
  edges <- 
    lapply(1:dim(phy$edge)[1], 
           function(i){
            edge_id <- nexml_id("e")
            source <- node_ids[as.character(phy$edge[i,1])]
            target <- node_ids[as.character(phy$edge[i,2])]
            e <- new("edge", 
                     source = source, 
                     target = target, 
                     id = edge_id,
                     about = paste0("#", edge_id))
           if(!is.null(phy$edge.length))
             e@length <- as.numeric(phy$edge.length[i])
           e
           }
  )
  edges <- new("ListOfedge", edges)
  ## Generate the ListOfnode made of "node" objects
  ## In doing so, generate otu_id numbers for tip nodes
  nodes <- lapply(unique(as.numeric(phy$edge)), function(i){
    node_id <- node_ids[as.character(i)] 
    if(is.na(phy$tip.label[i]))
      new("node", id = node_id, about = paste0("#", node_id))
    else if(is.character(phy$tip.label[i])){
      otu_id <- otu_map[phy$tip.label[i]]
      new("node", 
          id = node_id, 
          about = paste0("#", node_id), 
          otu = otu_id)  
    }
  })
  ## FIXME how about naming non-tip labels?  
  nodes <- new("ListOfnode", nodes)

 

  ## Create the "tree" S4 object
  tree_id <- nexml_id("tree") 
  tree <- new("tree", 
      node = nodes, 
      edge = edges,
      'xsi:type' = 'nex:FloatTree',
      id = tree_id,
      about = paste0("#", tree_id))  
}



