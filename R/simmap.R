# simmap.R

# if(!is.null(phy$maps))
#    <simmap:reconstructions> 
#      <simmap:reconstruction character="cr1">
#        <simmap:stateChange id="sc1" length="0.4" state="s2"/>
#        <simmap:stateChange id="sc2" length="0.5" state="s1"/>
#      </simmap:reconstruction>
#    </simmap:reconstructions>

## FIXME write the characters/states block (but not matrix block) as well.  

#' @export 
#' @import XML
simmap_to_nexml <- function(phy, state_ids = NULL){

  ## if state ids are not given
  if(is.null(state_ids)){
    state_ids <- levels(as.factor(names(unlist(phy$maps))))
    names(state_ids) <- state_ids
  }
  ## Create the NeXML object
  nexml <- as(phy, "nexml")

  # Loop over all edges, adding the simmap annotation to each: 
  for(i in 1:length(nexml@trees[[1]]@tree[[1]]@edge)){
   ## FIXME check to assure this is always the correct order??

    ## Read the mapping of the current edge 
    edge_map <- phy$maps[[i]]

    ## Generate the list of XML "stateChange" nodes 
    mapping <- lapply(1:length(edge_map), function(j){
      ##  A node has an id, a length and a state 
      newXMLNode("stateChange", 
                 attrs = c(id = nexml_id("sc"),
                           length = format(edge_map[[j]]), # avoid silly floating point errors in char conversion(?) 
                           state = state_ids[[names(edge_map[j])]],
                 namespace = "simmap")
                 )
    })

    ## Insert the stateChange nodes into a block of 
    ##    <reconstructions>
    ##      <reconstruction> ..
    reconstructions <- 
      newXMLNode("reconstructions", 
                 newXMLNode("reconstruction",
                            .children = mapping, 
                            namespace = "simmap"),
                 namespace = "simmap",
                 namespaceDefinitions = c(simmap = 
      "https://github.com/ropensci/RNeXML/tree/master/inst/simmap"))


  ## Insert the reconstructions into a <meta> element in each nexml edge
  nexml@trees[[1]]@tree[[1]]@edge[[i]]@meta <- 
    c(meta(type = "LiteralMeta",
           property = "simmap:reconstructions",
           children = list(reconstructions)))
  }

  ## Return the entire nexml object
  nexml 
}


## Returns list of multiPhylo ...
#' @export 
nexml_to_simmap <- function(nexml){

  ## Get the statemap, if available

  ## loop over trees blocks
  lapply(nexml@trees, function(trees){
    phys <- lapply(trees@tree, 
           tree_to_simmap, 
           get_otu_maps(nexml)[[trees@otus]]
          )
    names(phys) <- NULL
    class(phys) <- "multiPhylo"
    phys 
  })

}

# given the nexml tree:
tree_to_simmap <- function(tree, otus){
  maps <- lapply(tree@edge, function(edge){
    rep <- edge@meta[[1]]@children[[1]] # FIXME don't assume first element is a simmap:representations or first meta element
    stateChanges <- unname(xmlToList(rep[["reconstruction"]]))
    state_ids <- sapply(stateChanges, `[[`, "state")
    out <- as.numeric(sapply(stateChanges, `[[`, "length"))
    names(out) <- state_ids
    out
  })
  names(maps) <- NULL

  phy <- toPhylo(tree, otus)
  phy$maps <- maps

  ## create the rest of the maps elements

  ## Return phylo object
  phy
}


## Extend directly with XML representation instead of S4

#setClass("simmap:reconstructions",
#         slots = c(reconstruction = "ListOfreconstruction"))
#setClass("ListOfreconstruction", contains = "list")
#setClass("simmap:stateChange", contains = "IDTagged") 
#

