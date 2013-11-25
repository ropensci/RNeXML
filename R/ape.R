## These are really more like specific functions rather than generic methods .... 
setGeneric("toPhylo", function(tree, otus) standardGeneric("toPhylo"))

#' @import plyr
#' @import ape 
# Developer Note: This function simply calls `toPhylo` in the appropriate way 
# depending on the approriate number of trees in the nexml.  




####################################

add_tree <- function(nexml, phy){

  otu_map <- get_otu_map(nexml)
  reverse_otu_map <- names(otu_map)
  
  ## Generate the "ListOfedge" made of "edge" objects
  edges <- 
    lapply(1:dim(phy$edge)[1], 
           function(i){
            id <- nexml_id("e")
            e <- new("edge", 
                source = paste("n",phy$edge[i,1], sep=""), 
                target = paste("n",phy$edge[i,2], sep=""), 
                id = id,
                about = paste0("#", id))
           if(!is.null(phy$edge.length))
             e@length <- as.numeric(phy$edge.length[i])
           e
           }
  )
  edges <- new("ListOfedge", edges)
  ## Generate the ListOfnode made of "node" objects
  ## In doing so, generate otu_id numbers for tip nodes
  nodes <- lapply(unique(as.numeric(phy$edge)), function(i){
    id <- nexml_id("n")
    if(is.na(phy$tip.label[i]))
      new("node", id = id, about = paste0("#", id))

    else if(is.character(phy$tip.label[i])){
      otu_id <- nexml_id("t")
      new("node", id = id, 
          about = paste0("#", id), 
          otu = otu_id)  #phy$tip.label[i])  ## OTUs get abstract names
    }
  })
  ## FIXME how about naming non-tip labels?  
  nodes <- new("ListOfnode", nodes)

 

  ## Create the "tree" S4 object
  id <- nexml_id("tree") 
  tree <- new("tree", 
      node = nodes, 
      edge = edges,
      'xsi:type' = 'nex:FloatTree',
      id = id,
      about = paste0("#", id))  
})


##############################################################

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
          edges <- sapply(unname(tree@edge), 
                          function(x) 
                            c(source = unname(x@source), 
                              target = unname(x@target), 
                              length = if(identical(x@length, numeric(0)))
                                        NA 
                                       else 
                                         unname(x@length), 
                              id = unname(x@id)))
        else # no edge lengths 
          edges <- sapply(unname(tree@edge), 
                          function(x) 
                            c(source = unname(x@source), 
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





##################### phylo -> nexml #################################3


setAs("phylo", "nexml", function(from){
  treesid <- nexml_id("trees")
  trees = new("trees",
              id = treesid,
              about = paste0("#", id),
              tree = new("ListOftree", list( as(from, "tree") )))
  otus = as(from, "otus")
  otus@id = nexml_id("tax")
  otus@about = paste0("#", otus@id)
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



