


add_trees <- function(phylo, nexml=new("nexml")){

  nexml <- as(nexml, "nexml")

  # phylo is a single phylo
  # phylo is multiphylo
  # phylo is list of multiphylo
  

  
  add_tree(nexml, phylo)
}

## 
add_otu_from_phylo <- function(nexml, phy)

  otu <- lapply(unique(as.numeric(phy$edge)), function(i){
    if(is.character(phy$tip.label[i])){
      new("otu", 
          id = nodes[[i]]@id,
          about = paste("#", nodes[[i]]@id),
          label = phy$tip.label[i])
    } else {
      NULL 
    }
  })
  otu <- unname(plyr::compact(otu)) # drop the NULLs
 

