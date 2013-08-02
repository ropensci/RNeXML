#' Strucrure of ListOfMeta S4 class.
#'
#' Class will be hold list of NeXML metas.
#'
#' \describe{
#'    \item{list}{Contain list of meta class objects}
#'
#'  }
#' @name ListOfMeta-class
#' @rdname ListOfMeta-class
#' @exportClass ListOfMeta

setClass("ListOfMeta", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "meta")))
                          "not all elements are meta objects"
                       else
                         TRUE)