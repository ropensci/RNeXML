
## Conversions between matrix and characters node

#' Extract the character matrix
#'
#' @param nexml nexml object (e.g. from read.nexml)
#' @export
characters <- function(nexml){
# extract mapping between otus and taxon ids 
  maps <- get_otu_maps(nexml)
# loop over all character matrices 
  lapply(nexml@characters, function(characters){
    # extract data.frame from the S4 structure 
    dat <- extract_character_matrix(characters@matrix)

##  Convert states to symbols?  to labels? 
#    dat <- state_to_label(dat, characters@format)

    # Replace OTU ids with Taxon labels  
    dat <- otu_to_label(dat, maps[[characters@otus]])
    # Replace character id with state label?  
    dat <- character_to_label(dat, characters@format)
    dat
  })
  # name the character matrices by their ids  
} 


otu_to_label <- function(dat, otu_map){
  rownames(dat) <- otu_map[rownames(dat)]
  dat
}


character_to_label <- function(dat, format){ 

  ## Compute the mapping
  map <- sapply(format@char, function(char){
    if(length(char@label) > 0)
      label <- char@label
    else
      label <- char@id
    c(char@id, label)
  })
  out <- map[2,]
  names(out) <- map[1,]


  ## replace colnames with matching labels
  colnames(dat) <- out[colnames(dat)]
  dat
}


# For each character, find the matching `states` set.
# For that set, map each state id to the state symbol 

state_to_symbol <- function(dat, format){
  if(!isEmpty(format@states)){

    map_by_char <- lapply(format@char, function(char){ # loop over characters
      # name the list with elements as `states` sets by their ids 
      states <- format@states 
      ids <- sapply(states, function(states) states@id)
      names(sates) <- ids

      # Get the relevant states set matching the current character 
      map_states_to_symbols( states[[char@states]] )
    })  

    ids <- sapply(format@char, function(char) char@id)
    names(map_by_char) <- ids

    for(n in names(dat)){
      symbol <- map_by_char[[n]] 
      dat[[n]] <- symbol[dat[[n]]]
    }

    dat 
  } else {
    dat ## Nothing to do if we don't have a states list
  }
}

map_states_to_symbols <- function(states){
  map <- sapply(states@state,
                function(state){
    if(length(state@symbol) > 0)
      symbol <- state@symbol
    else
      symbol <- state@id
    c(state@id, symbol)
  })
  out <- map[2, ]
  names(out) <- map[1, ]
  out
}





#' @import reshape2
extract_character_matrix <- function(matrix){ 
  mat <- sapply(matrix@row, function(row) 
    sapply(row@cell, function(cell) 
      c(cell@char, cell@state)))
  mat <- t(mat)
  otu <- sapply(matrix@row, function(row) row@otu)
  colnames(mat) <- c("character", "state")
  mat <- data.frame(otu = otu, mat, check.names=FALSE, row.names=NULL)
  mat <- dcast(mat, otu ~ character, value.var = "state")

  # Move otus into rownames and drop the column 
  rownames(mat) <- mat[["otu"]]
  mat <- mat[-1]
  mat 
}

## Map state value to symbol for discrete traits?  





