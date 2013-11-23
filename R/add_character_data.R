#################### Write character matices into S4 #####################



add_character_data <- function(nexml, x, ...){

  ## Check types  & row names ##
  x <- check_characters(x) 
  ## divide matrix into discrete and continuous trait matrices, if necessary
  ## then write each as separate <characters> nodes: 
  ## x should now be a list of data.frames of common type

 

  ## Create the right number of characters nodes with ids.  
  nexml <- initialize_characters(nexml, length(x))

  for(i in 1:length(x)){
  ## See if an appropriate <otus> node already exists.  If not, create one.  
  nexml <- add_otu(nexml, x, i)
  ## Generate the char nodes with the appropriate mapping 
  nexml <- add_char(nexml, x, i)
  ## Generate the states nodes for each character
  nexml <- add_states(nexml, x, i)
  ## Generate the rows and cells
  nexml <- add_rows(nexml, x, i)  
  }

  nexml
}

initialize_characters <- function(nexml, n){
  cs_list <- lapply(1:n, function(i){
         id <- nexml_id("cs")
         new("characters", 
             id = id,
             about = paste0("#", id))
      })
  cs_list <- c(nexml@characters, cs_list)
  nexml@characters <-  new("ListOfcharacters", cs_list)
  nexml
}



otu_list <- function(to_add, prefix="ou"){
  lapply(to_add, 
         function(label){
         id <- nexml_id(prefix)
         new("otu", label=label, id =id, about = paste0("#", id))})
}


#' @include metadata_methods.R
add_otu <- function(nexml, x, i){

  new_taxa <- rownames(x[[i]]) 
  current_taxa <- get_taxa_list(nexml)

  if(length(current_taxa) == 0) # No otus exist, create a new node 
    nexml@otus <- new("ListOfotus", 
                      list(new("otus", 
                               otu = new("ListOfotu", 
                                         otu_list(new_taxa))
                               )))


  else {

  otu_pos <- lapply(current_taxa, function(current) match(new_taxa, current))

    if(any(is.na(unlist(otu_pos)))){ # We have missing taxa
      to_add <- new_taxa[sapply(otu_pos, is.na)]  
      nexml@otus[[1]]@otu <- new("ListOfotu", c(nexml@otus[[1]]@otu, otu_list(to_add, "ou_char"))) ## FIXME hack to make sure new ids are 'unique', 

    } # else # all taxa matched, so we're all set
  }

  nexml
}


# Turns char names into char nodes 
add_char <- function(nexml, x, i = 1){
  char_labels <- colnames(x[[i]])
  char_list <- 
    lapply(char_labels, function(lab){
      id <- nexml_id("cr")
      char <- new("char", 
                  id = id, 
                  about = paste0("#", id),
                  label = lab) 
      })
  nexml@characters[[i]]@format@char <- new("ListOfchar", char_list)
  nexml 
}



add_states <- function(nexml, x, i = 1){
  nchars <- length(x[[i]])
  char <- nexml@characters[[i]]@format@char

  states_list <- lapply(1:nchars, function(j){
    lab <- char[[j]]@label
    lvls <- levels(x[[i]][lab])
    id <- nexml_id("ss")
    states <- 
    new("states", 
        id = id,
        about = paste0("#", id),
        state = new("ListOfstate",  
        lapply(lvls, function(lvl){
          new("state", 
              id=nexml_id("s"),
              symbol = lvl)
        }))
    )
  })
  nexml@characters[[i]]@format@states <- new("ListOfstates", states_list)
  # Add the states's id to char
  for(j in 1:nchars)
    nexml@characters[[i]]@format@char[[j]]@states <- states_list[[j]]@id
  
  nexml
}      


## Assumes that otu ids have already been added to the nexml 
add_rows <- function(nexml, x, i = 1){

  X <- x[[i]]
  taxa <- rownames(X)
  char_labels <- colnames(X)
  cs <- nexml@characters[[i]]@id

  otu_map <- get_otu_maps(nexml)[[cs]]
  char_map <- get_char_maps(nexml)[[cs]] 
  state_map <- get_state_maps(nexml)[[cs]] 

  mat <- 
    new("obsmatrix", 
        row = new("ListOfrow", 
    lapply(taxa, 
      function(taxon){
        id = nexml_id("rw")
        new("row",
            id = id,
            about = paste0("#", id),
            label = taxon,
            otu = otu_map[taxon],
            cell = new("ListOfcell",  
             lapply(char_labels, function(char){
                    state <- X[taxon,char] # unmapped
                    char_id <- char_map[char]
                    if(!is.null(state_map[[char_id]]))
                      state <- state_map[[char_id]][state]
                    new("cell",
                        char = char_id,
                        state = state)
                       }))
           )
        }))
    )
  nexml@characters[[i]]@matrix <- mat

}


check_characters <- function(x){
#
#  if(is(x, "matrix")){
#    x <- as.data.frame(x)
#  } else if(is(x, "data.frame")) {
#    rownames(x) 
#    colnames(x)
#
#  } else if(is(x, "list")) {
#    if(is(x[[1]], "matrix"))
#    if(is(x[[1]], "data.frame"))
#  } else {
#    stop("x must be a matrix or a data.frame with characters as column names and taxonomic units as species names")
#  }
  x
}
