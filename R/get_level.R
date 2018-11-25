
## Should be all element names (unless they are also attribute names!), since we only want attribute names 
## (alternately we should define to only grab possible attribute names..)
SKIP = c("meta", "children", "member", "row", "cell", "seq", "matrix", "format", "names")


#' get_level
#' 
#' get a data.frame of attribute values of a given node
#' 
#' @param nex a nexml object
#' @param level a character vector indicating the class of node, see details
#' 
#' @return Returns the attributes of specified class of nodes as a data.frame
#' 
#' @details level should be a character vector giving the path to the specified node
#' group.  For instance, `otus`, `characters`, and `trees` are top-level blocks (e.g.
#' child nodes of the root nexml block), and can be specified directly.  To get metadata
#' for all "char" elements from all characters blocks, you must specify that `char` nodes
#' are child nodes to `character` nodes: e.g. `get_level(nex, "characters/char")`,
#' or similarly for states: `get_level(nex, characters/states)`.  
#' 
#' The return object is a data frame whose columns are the attribute names of the elements
#' specified. The column names match the attribute names except for "id" attribute, for which the column
#' is renamed using the node itself. (Thus <otus id="os2"> would be rendered in a data.frame with column
#' called "otus" instead of "id"). Additional columns are
#' added for each parent element in the path; e.g. get_level(nex, "otus/otu") would include a column
#' named "otus" with the id of each otus block.  Even though the method always returns the data frame 
#' for all matching nodes in all blocks, these ids let you see which otu values came from which 
#' otus block.  This is identical to the function call `get_taxa()`.  
#' Similarly, `get_level(nex, "otus/otu/meta")` would return additional columns 'otus' and 
#' also a column, 'otu', with the otu parent ids of each metadata block.  (This is identical to a 
#' function call to `get_metadata`).  This makes it easier to join data.frames as well, see examples
#' 
#' @export
#' @importFrom dplyr select
get_level <- function(nex, level){
  lvl <- strsplit(level, "/")[[1]]
  out <- recursion(1, lvl)(nex) %>% 
    dplyr::select_(quote(-nexml))

  ## drop columns that are all-na?
#  all_na <- sapply(out, function(x) all(is.na(x)))
#  out <- out[!all_na]  
  out
}


## Trick to apply nodelist_to_df iteratively
closure <- function(level, fun) 
  function(node) nodelist_to_df(node, level, fun)

recursion <- function(i, level){
  if(i < length(level))
    closure(level[i], recursion(i+1, level))
  else
    closure(level[i], attributes_to_row)
}



## Assumes slot(node, element) is a list
#' @importFrom lazyeval interp
#' @importFrom dplyr bind_rows coalesce mutate mutate_ %>%
#' @importFrom uuid UUIDgenerate
nodelist_to_df <- function(node, element, fn, nodeId=NA){
  dots <- setNames(
    list(lazyeval::interp(~x, x = if (is.na(nodeId)) node_id(node) else nodeId)),
    idRefColName(node))
  nodelist <- slot(node, element)
  if(is.list(nodelist)){ ## node has a list of elements
    out <- suppressWarnings(lapply(nodelist, fn)) %>% dplyr::bind_rows()
    if (length(nodelist) > 0 && all(sapply(nodelist, is, "meta"))) {
      # meta elements may have nested meta elements, retrieve these here too
      ids <- sapply(nodelist,
                    function(n) if (length(n@children) > 0) uuid::UUIDgenerate() else NA)
      if (length(ids) > 0 && any(!is.na(ids))) {
        mout <- dplyr::mutate(out, "Meta" = coalesce_(out$LiteralMeta,
                                                      out$ResourceMeta,
                                                      ids))
        nested <- mapply(function(n, id) nodelist_to_df(n, "children", fn, id),
                         n = nodelist,
                         id = mout[,"Meta"])
        if (is.null(names(nested))) nested <- as.data.frame(nested[,1])
        out <- dplyr::bind_rows(mout, nested)
      }
    }
    dplyr::mutate_(out, .dots = dots) -> out
  } else { ## handle case when node has only one element
    fn(nodelist) %>%
      dplyr::mutate_(.dots = dots)
  }
}

node_id <- function(node){
  if("id" %in% slotNames(node))
    slot(node, "id")
  else
    "root"
}

idRefColName <- function(node){
  clname <- class(node)
  super <- names(getClass(clname)@contains)
  # meta elements can be nested, avoid clobbering the ID column with the IDREF
  if (length(super) > 0 && (super[1] == "meta"))
    super[1]
  else
    clname
}

attributes_to_row <- function(node){
  who <- slotNames(node)
  
  ## Avoid things that are not attributes:
  types <- getSlots(class(node)) # actual slot values may be derived types
  who <- who[ types %in% c("character", "integer", "numeric", "logical") ]
  # the "about" attribute is only needed for RDF extraction
  who <- who[!(who %in% c("names","about"))]
  
  ## Extract attributes, use NAs for numeric(0) / character(0) values
  tmp <- sapply(who, function(x) slot(node, x))
  tmp[sapply(tmp,length) < 1] <- NA
  
  ## Coerce into a row of a data.frame & rename id column to match class
  out <- data.frame(as.list(tmp), stringsAsFactors=FALSE) 
  if("id" %in% who)
    out <- dplyr::rename_(out, .dots = setNames("id", class(node)))
  
  out
}
