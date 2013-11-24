
## Conversions between matrix and characters node

#' Extract the character matrix
#'
#' @param nexml nexml object (e.g. from read.nexml)
#' @export
get_characters_list <- function(nexml){
# extract mapping between otus and taxon ids 
  maps <- get_otu_maps(nexml)
# loop over all character matrices 
  out <- lapply(nexml@characters, function(characters){
    # extract data.frame from the S4 structure 
    dat <- extract_character_matrix(characters@matrix)
    # Convert states to symbols
    dat <- state_to_symbol(dat, characters@format)
    # Replace OTU ids with Taxon labels  
    dat <- otu_to_label(dat, maps[[characters@otus]])
    # Replace character id with state label?  
    dat <- character_to_label(dat, characters@format)

    # Make numeric data class numeric, discrete data class discrete
    type <- slot(characters, 'xsi:type') # check without namespace too?
    if(type == "nex:ContinuousCells")
      for(i in length(dat))                 ## FIXME this could be faster/more elegant, no?
        dat[[i]] <- as.numeric(dat[[i]])
    else 
      for(i in length(dat))
        dat[[i]] <- factor(dat[[i]])
    dat
  })
  # name the character matrices by their labels if available, otherwise, by id.    
  names(out) <- name_by_id_or_label(nexml@characters) 
  out
} 

name_by_id <- function(x)
  unname(sapply(x, function(i) if(length(i@id)>0) i@id else NULL))
name_by_id_or_label <- function(x)
  unname(sapply(x, function(i) if(length(i@label)>0) i@label else i@id))


#' @export
get_characters <- function(nexml){
   list_chars <- get_characters_list(nexml)
   if(identical_rownames(list_chars))
     out <- do.call(cbind, list_chars) ## This could probably be more intelligent
   else { 
     out <- ldply(list_chars)  ## This could definitely be more intelligent
     n <- sapply(list_chars, rownames)
     for(i in 1:length(colnames(n)))
       out[[1]][out[[1]] == colnames(n)[i]] <- n[,i]
     out  ## FIXME figure out how to collapse replicate taxa
   }
   out
}


#' Get character data.frame, accepts either nexml object, or a list of data.frames
#' 
#' @examples \dontrun{
#' # library(RNeXML)
#' f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
#' nex <- read.nexml(f)
#' RNeXML:::get_characters(nex)
#' 
#' # with different row.names
#' char_list <- get_characters_list(nex)
#' row.names(char_list[[1]])[1:3] <- c("taxon_18","taxon_20","taxon_30")
#' RNeXML:::get_characters(char_list
#' } 
#get_characters <- function(input){
#  if(inherits(input, "nexml")){
#    list_chars <- get_characters_list(input)
#  } else { list_chars <- input }
#  mrecurse <- function(dfs, ...){
#    merge(dfs, ..., by='row.names',  all = TRUE, sort = FALSE)
#  }
#  tmp <- Reduce(mrecurse, list_chars)
#  row.names(tmp) <- tmp[,1]
#  tmp[,-1]
#}
#
# for lists only 
identical_rownames <- function(x) all(sapply(lapply(x, rownames), identical, rownames(x[[1]])))



otu_to_label <- function(dat, otu_map){
  rownames(dat) <- otu_map[rownames(dat)]
  dat
}



map_chars_to_label <- function(format){
  map <- sapply(format@char, function(char){
    if(length(char@label) > 0)
      label <- char@label
    else
      label <- char@id
    c(char@id, label)
  })
  out <- map[2,]
  names(out) <- map[1,]
  out
}

character_to_label <- function(dat, format){ 
  ## Compute the mapping
  map <- map_chars_to_label(format)
  ## replace colnames with matching labels
  colnames(dat) <- map[colnames(dat)]
  dat
}

get_char_maps <- function(nexml){
  map <- lapply(nexml@characters, function(characters)
         map_chars_to_label(characters@format))
  names(map) <- name_by_id(nexml@characters)
  map
}

get_state_maps <- function(nexml){
  map <- lapply(nexml@characters, function(characters){
    if(!isEmpty(characters@format@states))
      map_state_to_symbol(characters@format)
    else
     NULL
  })    
  names(map) <- name_by_id(nexml@characters)
  map
}


# For each character, find the matching `states` set.
# For that set, map each state id to the state symbol 
map_state_to_symbol <- function(format){
  # loop over characters
  map <- lapply(format@char, function(char){ 
      # name the list with elements as `states` sets by their ids 
      states <- format@states 
      ids <- sapply(states, function(states) states@id)
      names(states) <- ids
      # Get the relevant states set matching the current character 
      map_states_to_symbols( states[[char@states]] )
    })  
    names(map) <- name_by_id(format@char)
    map
}

state_to_symbol <- function(dat, format){
  if(!isEmpty(format@states)){
    map_by_char <- map_state_to_symbol(format)
    for(n in names(dat)){
      symbol <- map_by_char[[n]] 
      dat[[n]] <- symbol[dat[[n]]]
    }
    dat 
  } else {
    dat ## Nothing to do if we don't have a states list
  }
}


## Um, perhaps we should make all these as functions of nexml objects 

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






