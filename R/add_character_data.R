#################### Write character matices into S4 #####################


#' Add character data to a nexml object
#'
#' @param x character data, in which character traits labels are column names
#'        and taxon labels are row names.  x can be in matrix or data.frame
#'        format.  
#' @include classes.R
#' @export 
add_character_data <- function(x, nexml = new("nexml"), ...){

  # FIXME does it make sense to take a phylo object here as an option?  
  # If so, perhaps don't call the argument 'nexml'.  
  # If not, then we don't really need this conversion. (maybe a type-check instead).   
  nexml <- as(nexml, "nexml")

  ## Check types  & row names ##
  x <- format_characters(x) 

  nexml <- add_characters(nexml, x)
  for(i in 1:length(x)){
    nexml <- add_otu(nexml, x, i)
    nexml <- add_char(nexml, x, i)
    nexml <- add_states(nexml, x, i)
  }
  for(i in 1:length(x)){
    nexml <- add_rows(nexml, x, i)  
  }

  nexml
}

add_characters <- function(nexml, x){
  n <- length(x)
  cs_list <- lapply(1:n, function(i){
         uid <- nexml_id("cs")
         characters <- new("characters", 
             id = uid,
             about = paste0("#", uid))
         if(class(x[[i]][[1]] == "numeric")
           type <- "nex:ContinuousCells"
         else
           type <- "nex:StandardCells"
         slot(characters, "xsi:type") <- type  
         characters
      })
  cs_list <- c(nexml@characters, cs_list)
  nexml@characters <-  new("ListOfcharacters", cs_list)
  nexml
}



otu_list <- function(to_add, prefix="ou"){
  lapply(to_add, 
         function(label){
         uid <- nexml_id(prefix)
         new("otu", label=label, id =uid, about = paste0("#", uid))})
}


#' @include metadata_methods.R
add_otu <- function(nexml, x, i){

  new_taxa <- rownames(x[[i]]) 
  current_taxa <- get_taxa_list(nexml)

  if(length(current_taxa) == 0) { # No otus exist, create a new node 
    id <- nexml_id("os")
    nexml@otus <- new("ListOfotus", 
                      list(new("otus",
                               id = id,
                               about = paste0("#", id),
                               otu = new("ListOfotu", 
                                         otu_list(new_taxa))
                               )))

  } else {

  otu_pos <- lapply(current_taxa, function(current) match(new_taxa, current))

    if(any(is.na(unlist(otu_pos)))){ # We have missing taxa
      to_add <- new_taxa[sapply(otu_pos, is.na)]  
      nexml@otus[[1]]@otu <- new("ListOfotu", c(nexml@otus[[1]]@otu, otu_list(to_add, "ou_char"))) ## FIXME hack to make sure new ids are 'unique', 
    } # else # all taxa matched, so we're all set
  }
  ## Add the otus id to the characters node
  nexml@characters[[i]]@otus <- nexml@otus[[1]]@id

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
  # don't ctreate a states node if data is numeric
  if(all(sapply(x[[i]], is.numeric)))
    nexml
  else { 
    nchars <- length(x[[i]])
    char <- nexml@characters[[i]]@format@char
    states_list <- lapply(1:nchars, function(j){
      lab <- char[[j]]@label
      lvls <- levels(x[[i]][[lab]])
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
  nexml
}      


## Assumes that otu ids have already been added to the nexml 
add_rows <- function(nexml, x, i = 1){

  X <- x[[i]]
  taxa <- rownames(X)
  char_labels <- colnames(X)
  cs <- nexml@characters[[i]]@id   
  os <- nexml@characters[[i]]@otus 
  otu_map <- get_otu_maps(nexml)[[os]]
  char_map <- get_char_maps(nexml)[[cs]] 
  state_map <- get_state_maps(nexml)[[cs]]

  reverse_otu_map <- names(otu_map) 
  names(reverse_otu_map) <- otu_map
  reverse_char_map <- names(char_map) # name is label, value is the id
  names(reverse_char_map) <- char_map

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
            otu = reverse_otu_map[taxon],
            cell = new("ListOfcell",  
             lapply(char_labels, function(char){
                    state <- X[taxon,char] # unmapped
                    char_id <- reverse_char_map[[char]]
                    if(!is.null(state_map))
                      state <- state_map[[char_id]][state]
                    new("cell",
                        char = char_id,
                        state = as.character(state))
                       }))
           )
        }))
    )
  nexml@characters[[i]]@matrix <- mat
  nexml
}

# divide a data.frame into a list of data.frames, in which each has only a unique column class
split_by_class <- function(x){
    col.classes <- sapply(x, class)
    if(all(sapply(col.classes, identical, col.classes[1])))
      x <- list(x)
    else {
      ## split into numerics and non-numerics 
      cts <- which(col.classes=="numeric")
      discrete <- which(col.classes!="numeric")
      x <- list(x[cts], c[discrete])
    }

  x
}

## divide matrix into discrete and continuous trait matrices, if necessary
## then write each as separate <characters> nodes: 
## x should now be a list of data.frames of common type
format_characters <- function(x){

  ## Actually useful conversions ##
  ## Matrices are either all-numeric or all-character class, so no risk of mixed discrete and continous states.  
  if(is(x, "matrix")){
    x <- list(as.data.frame(x))
  ## Data.frames can mix discrete and continous states, so we need to seperate them
  } else if(is(x, "data.frame")) {
    x <- split_by_class(x)

  #### Ugh, this next bit isn't pretty.  Maybe we should  just hope lists are formatted correctly, e.g. come from get_character_list. 

  ## If we're getting a list with matrices, coerce them into data.frames and hope for the best.  
  ## If the list has data.frames, check that each one has consistent class type.  
  ## Otherwise, panic.  
  } else if(is(x, "list")) {
    for(i in 1:length(x)){
      if(is(x[[i]], "matrix"))
        x[[i]] <- as.data.frame(x[[i]])  ## A list of matrices we can make into a list of data.frames...
#      else if(is(x[[i]], "data.frame")){
#        col.classes <- sapply(x, class)
#        if(!all(sapply(col.classes, identical, col.classes[1]))) 
#          stop("data.frames must have a consistent class")
#      } else
#        stop("if x is a list, must contain only data.frames of a single type, or matrices")
    }
  ## Someone didn't even try to read the documentation...
  } else { 
    stop("x must be a matrix, data.frame, or list thereof")
  }
  ## Let's just hope folks read the documentation and have 
  ## row names as taxa and column names as be character traits.  
  ## Kinda hard to check that for sure?  

  ## return the updated object: a list of data.frames
  x
}


