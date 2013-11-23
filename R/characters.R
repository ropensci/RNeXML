
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
  id <- sapply(nexml@characters, function(characters) if(length(characters@label)>0) characters@label else characters@id)
  names(out) <- id
  out
} 

# get_characters <- function(nexml){
#   list_chars <- get_characters_list(nexml)
#   if(identical_rownames(list_chars))
#     out <- do.call(cbind, list_chars) ## This could probably be more intelligent
#   else { 
#     out <- ldply(list_chars)  ## This could definitely be more intelligent
#     n <- sapply(list_chars, rownames)
#     for(i in 1:length(colnames(n)))
#       out[[1]][out[[1]] == colnames(n)[i]] <- n[,i]
#     out  ## FIXME figure out how to collapse replicate taxa
#   }
#   out
# }


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
get_characters <- function(input){
  if(inherits(input, "nexml")){
    list_chars <- get_characters_list(input)
  } else { list_chars <- input }
  mrecurse <- function(dfs, ...){
    merge(dfs, ..., by='row.names',  all = TRUE, sort = FALSE)
  }
  tmp <- Reduce(mrecurse, list_chars)
  row.names(tmp) <- tmp[,1]
  tmp[,-1]
}

# for lists only 
identical_rownames <- function(x) all(sapply(lapply(x, rownames), identical, rownames(x[[1]])))



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
      names(states) <- ids

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
  ## See if an appropriate <otus> node already exists


  ## Map otus to otu id numbers (existing or new) 

  ## Map column-name characters to character id values

  ## Generate the char nodes with the appropriate mapping 

  ## Map state symbols to states
  nexml <- symbols_to_state_ids(nexml, x, i) 
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
  nexml@characters <- c(nexml@characters, cs_list)
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
                  about = paste0("#", char@id),
                  label = lab) 
      })
  nexml@characters[[i]]@format@char <- char_list
  nexml 
}



add_states <- function(nexml, x, i = 1){
    nchars <- length(x[[i]])
    for(j in 1:nchars){
      lab <- char@label
      lvls <- levels(x[[i]][lab])
      id <- nexml_id("ss")
      states <- 
      new("states", 
          id = id,
          about = paste0("#", id),
          state = 
          lapply(lvls, function(lvl){
            new("state", 
                id=nexml_id("s"),
                symbol = lvl)
          })
        )
        nexml@characters[[i]]@format@char[[j]]@states <- states
    }      
  nexml
}


## Assumes that otu ids have already been added to the nexml 
add_rows <- function(nexml, x, i = 1){

  X <- x[[i]]
  taxa <- rownames(X)
  char_labels <- colnames(X)
  ## From i, figure out the id number 
  cs <- nexml@characters[[i]]@id

  otu_map <- get_otu_maps(nexml)[[cs]]

  state_map <- 
  char_map <-   

  mat <- 
    new("obsmatrix", 
        row = lapply(taxa, 
      function(taxon){
        id = nexml_id("rw")
        new("row",
            id = id,
            about = paste0("#", id),
            label = taxon,
            otu = otu_map[taxon],
            cell = 
             lapply(char_labels, function(char){
                    state <- X[taxon,char] # unmapped 
                    new("cell",
                        char = char_map[char],
                        state = state_map[state])
                       })
           )
        })
    )
  nexml@characters[[i]]@matrix <- mat

}


check_characters <- function(x){

  if(is(x, "matrix")){
    x <- as.data.frame(x)
  } if(is(x, "data.frame")) {
    rownames(x) 
    colnames(x)

  } if(is(x, "list")) {
    if(is(x[[1]], "matrix"))
    if(is(x[[1]], "data.frame"))
  } else {
    stop("x must be a matrix or a data.frame with characters as column names and taxonomic units as species names")
  } 
}
