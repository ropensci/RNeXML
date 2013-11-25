
## Helper functions 
name_by_id <- function(x)
  unname(sapply(x, function(i) if(length(i@id)>0) i@id else NULL))
name_by_id_or_label <- function(x)
  unname(sapply(x, function(i) if(length(i@label)>0) i@label else i@id))


