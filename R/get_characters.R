#' Get character data.frame from nexml
#' 
#' @param nex a nexml object 
#' @param rownames_as_col option to return character matrix rownames (with taxon ids) as it's own column in the 
#' data.frame. Default is FALSE for compatibility with geiger and similar packages.
#' @param otu_id logical, default FALSE. return a column with the 
#'  otu id (for joining with otu metadata, etc)
#' @param otus_id logical, default FALSE. return a column with the 
#'  otus block id (for joining with otu metadata, etc)
#' @param include_state_types logical, default FALSE. whether to also return a 
#'  matrix of state types (with values standard, polymorphic, and uncertain)
#' @return the character matrix as a data.frame, or if `include_state_types` is
#'  TRUE a list of two elements, `characters` as the character matrix, and
#'  `state_types` as a matrix of state types. Both matrices will be in the same
#'  ordering of rows and columns.
#' @details RNeXML will attempt to return the matrix using the NeXML taxon (otu) labels to name the rows
#'  and the NeXML char labels to name the traits (columns).  If these are unavailable or not unique, the NeXML
#'  id values for the otus or traits will be used instead.
#' @importFrom tidyr spread
#' @importFrom dplyr left_join select mutate mutate_at matches any_of
#' @importFrom stringr str_replace
#' @importFrom stats setNames
#' @export
#' @examples
#' \dontrun{
#' # A simple example with a discrete and a continous trait
#' f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
#' nex <- read.nexml(f)
#' get_characters(nex)
#' 
#' # A more complex example -- currently ignores sequence-type characters
#' f <- system.file("examples", "characters.xml", package="RNeXML")
#' nex <- read.nexml(f)
#' get_characters(nex)
#' 
#' # if polymorphic or uncertain states need special treatment, request state
#' # types to be returned as well:
#' f <- system.file("examples", "ontotrace-result.xml", package="RNeXML")
#' nex <- read.nexml(f)
#' res <- get_characters(nex, include_state_types = TRUE)
#' row.has.p <- apply(res$state_types, 1, 
#'                    function(x) any(x == "polymorphic", na.rm = TRUE))
#' col.has.p <- apply(res$state_types, 2, 
#'                    function(x) any(x == "polymorphic", na.rm = TRUE))
#' res$characters[row.has.p, col.has.p, drop=FALSE] # polymorphic rows and cols
#' res$characters[!row.has.p, drop=FALSE] # drop taxa with polymorphic states
#' # replace polymorphic state symbols in matrix with '?'
#' m1 <- mapply(function(s, s.t) ifelse(s.t == "standard", s, "?"), 
#'              res$characters, res$state_types)
#' row.names(m1) <- row.names(res$characters)
#' m1
#' }
get_characters <- function(nex, 
                           rownames_as_col=FALSE, otu_id=FALSE, otus_id=FALSE,
                           include_state_types=FALSE) {
  
  drop = c("about","xsi.type","format")
  
  otus <- get_level(nex, "otus/otu") %>% 
    dplyr::select(!any_of(drop)) %>%
    optional_labels(id_col = "otu")
  
  char <- get_level(nex, "characters/format/char") %>% 
    dplyr::select(!any_of(drop)) %>%
    optional_labels(id_col = "char")
  
  ## Rows have otu information
  rows <- get_level(nex, "characters/matrix/row") %>% 
    dplyr::select(c("otu", "row"))

  cellLevel <- get_level(nex, "characters/matrix/row/cell")
  cells <- cellLevel %>% 
    dplyr::select(c("char", "state", "row")) %>%
    dplyr::left_join(rows, by = "row")
    
  characters <- get_level(nex, "characters")
  
  ## States, not including polymorphic or uncertain states
  states <- get_level(nex, "characters/format/states/state") %>%
    dplyr::mutate(state.type = "standard")
  
  ## Include polymorphic and uncertain states.
  polymorph <- get_level(nex, "characters/format/states/polymorphic_state_set")
  uncertain <- get_level(nex, "characters/format/states/uncertain_state_set")
  if(dim(polymorph)[1] > 0) {
    ## For the result to work correctly for joining, the ID column needs to
    ## be uniformly named "state".
    polymorph <- polymorph %>%
      dplyr::mutate(state.type = "polymorphic") %>%
      dplyr::rename(state = "polymorphic_state_set")
    # StandardStates have integer symbols, but polymorphic state sets typically
    # do not. If states are DNA, then states will already be character type, and
    # then changing to character should presumably have no effect.
    states <- states %>% dplyr::mutate_at(c("symbol"), as.character) %>%
                         dplyr::bind_rows(polymorph)
  }
  if(dim(uncertain)[1] > 0) {
    ## Same issue for uncertain states.
    uncertain <- uncertain %>%
      dplyr::mutate(state.type = "uncertain") %>%
      dplyr::rename(state = "uncertain_state_set")
    states <- states %>% dplyr::mutate_at(c("symbol"), as.character) %>%
                         dplyr::bind_rows(uncertain)
  }
  states <- dplyr::select(states, !any_of(drop))


  if(dim(states)[1] > 0) 
    cells <- cells %>% 
    dplyr::left_join(states, by = c("state"))  %>% 
    dplyr::select(c("char", "symbol", "otu", "state", "state.type"))
  
  ## Join the matrices.  Note that we select unique column names after each join to avoid collisions
  cells <- cells %>% 
    dplyr::left_join(char, by = c("char")) %>% 
    dplyr::rename(c("trait" = "label")) %>% 
    dplyr::left_join(otus, by = c("otu")) %>%
    dplyr::rename(c("taxa" = "label"))
  # character state matrix with symbols:
  cells %>%
    na_symbol_to_state() %>% 
    dplyr::select(c("taxa", "symbol", "trait", "otu", "otus")) %>% 
    tidyr::spread("trait", "symbol") ->
    out
  # state type matrix
  if(dim(states)[1] == 0) {
    # this can be the case for example for continuous states
    cells <- dplyr::mutate(cells, state.type = "standard")
  }
  cells %>%
      dplyr::select(c("taxa", "state.type", "trait", "otu", "otus")) %>%
      dplyr::mutate_at("state.type", as.factor) %>%
      tidyr::spread("trait", "state.type") ->
      state_types

  ## Identify the class of each column and reset it appropriately
  cellclass <- function(x){
    x %>%
    stringr::str_replace(".*ContinuousCells", "numeric") %>%
    stringr::str_replace(".*StandardCells", "integer")
  }
  
  type <-
    cellLevel  %>% 
    dplyr::select(!any_of(drop)) %>%
    dplyr::left_join(characters, by = "characters") %>%
    dplyr::select(c( "xsi.type", "char", "characters")) %>%
    dplyr::left_join(char, by = c("char")) %>%
    dplyr::select(c("label", "xsi.type")) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(class = cellclass(xsi.type))
  
  for(i in 1:dim(type)[1]) {
    if (all(state_types[[type$label[i]]] == "standard", na.rm = TRUE)) {
      class(out[[type$label[i]]]) <- type$class[i]
    }
  }
  
  ## drop unwanted columns if requested (default)
  if(!otu_id){
    out <- dplyr::select(out, !"otu")
    if (include_state_types)
      state_types <- dplyr::select(state_types, !"otu")
  }
  if(!otus_id){
    out <- dplyr::select(out, !"otus")
    if (include_state_types)
      state_types <- dplyr::select(state_types, !"otus")
  }
  if(!rownames_as_col){
    taxa <- out$taxa
    out <- as.data.frame(dplyr::select(out, !"taxa"))
    rownames(out) <- taxa
    if (include_state_types) {
      state_types <- as.data.frame(dplyr::select(state_types, !"taxa"))
      rownames(state_types) <- taxa
    }
  }
  if (include_state_types) {
    out <- list(characters = out, state_types = state_types)
  }
  
  out
}

## If 'label' column is missing, create it from 'id' column
## if label exists but has missing or non-unique values, also use ids instead
optional_labels <- function(df, id_col = "id"){
  who <- names(df)
  if(! "label" %in% who)
    df$label <- df[[id_col]]
  if(length(unique(df$label)) < length(df$label))
    df$label <- df[[id_col]]
  df
}

## Continuous traits have the values in "state" column, whereas 
## for discrete states we return the value of the "symbol" column 
na_symbol_to_state <- function(df){
  if(is.null(df$symbol))
    df$symbol <- NA
  df$symbol[is.na(df$symbol)] <- suppressWarnings(as.numeric(df$state[is.na(df$symbol)]))
  df
  }

## silence R CHECK warning
xsi.type <- NULL