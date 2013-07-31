#' Strucrure of ListOfEdge S4 class.
#'
#' Class will be hold list of NeXML edges.
#'
#' \describe{
#'    \item{list}{Contain list of edge class objects}
#'
#'  }

#' @name ListOfEdge-class
#' @rdname ListOfEdge-class
#' @exportClass ListOfEdge

setClass("ListOfEdge", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "edge")))
                          "not all elements are edge objects"
                       else
                         TRUE)