#' Get character data.frame from nexml
#' 
#' @param nex a nexml object 
#' @param rownames_as_col option to return character matrix rownames (with taxon ids) as it's own column in the data.frame. Default is FALSE for compatibility with geiger and similar packages.
#' @param otu_id logical, default FALSE. return a column with the 
#'  otu id (for joining with otu metadata, etc)
#' @param otu_id logical, default FALSE. return a column with the 
#'  otus block id (for joining with otu metadata, etc)
#'  @return the character matrix as a data frame
#' @importFrom tidyr spread
#' @importFrom dplyr left_join select_
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
#' }
get_characters <- function(nex, rownames_as_col=FALSE, otu_id = FALSE, otus_id = FALSE){
  
  otus <- get_level(nex, "otus/otu") %>% 
    select_(quote(-about), quote(-xsi.type)) %>%
    optional_labels()
  
  char <- get_level(nex, "characters/format/char") %>% 
    select_(quote(-about), quote(-xsi.type)) %>%
    optional_labels()
  
  rows <- get_level(nex, "characters/matrix/row") %>% 
    dplyr::select_(.dots = c("otu", "id"))
  
  cells <- get_level(nex, "characters/matrix/row/cell") %>% 
    dplyr::select_(.dots = c("char", "state", "row")) %>% 
    dplyr::left_join(rows, by = c("row" = "id"))
  
  ## States, including polymorphic states (or uncertain states)
  states <- get_level(nex, "characters/format/states/state") 
  
  polymorph <- get_level(nex, "characters/format/states/polymorphic_state_set") 
  uncertain <- get_level(nex, "characters/format/states/uncertain_state_set") 
  if(dim(polymorph)[1] > 0)
    states <- dplyr::bind_rows(states, polymorph)
  if(dim(uncertain)[1] > 0)
    states <- dplyr::bind_rows(states, uncertain)
  states <- select_(states, quote(-about), quote(-xsi.type), quote(-format))
  
  ## FIXME what about missing labels?
  cells %>% 
    dplyr::left_join(states, by = c("state" = "id")) %>% 
    dplyr::select_(.dots = c("char", "symbol", "otu")) %>% 
    dplyr::left_join(char, by = c("char" = "id")) %>% 
    dplyr::select_(.dots = c("label", "symbol", "otu")) %>% 
    dplyr::rename_(.dots = c("trait" = "label")) %>% 
    dplyr::left_join(otus, by = c("otu" = "id")) %>%
    dplyr::rename_(.dots = c("taxa" = "label")) %>%
    dplyr::select_(.dots = c("taxa", "symbol", "trait", "otu", "otus")) %>%
    tidyr::spread("trait", "symbol") ->
    out
  
  if(!otu_id){
    out <- dplyr::select_(out, quote(-otu))
  }
  if(!otus_id){
    out <- dplyr::select_(out, quote(-otus))
  }
  if(!rownames_as_col){
    taxa <- out$taxa
    out <- dplyr::select_(out, quote(-taxa))
    out <- as.data.frame(out)  
    rownames(out) <- taxa
    out
  }
  out
}

## If 'label' column is missing, create it from 'id' column
optional_labels <- function(df){
  who <- names(df)
  if(! "labels" %in% who)
    df$labels <- df$id
  df
}
