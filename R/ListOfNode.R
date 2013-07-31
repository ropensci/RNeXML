#' Strucrure of ListOfNode S4 class.
#'
#' Class will be hold list of NeXML nodes.
#'
#' \describe{
#'    \item{list}{Contain list of node class objects}
#'
#'  }

#' @name ListOfNode-class
#' @rdname ListOfNode-class
#' @exportClass ListOfNode

setClass("ListOfNode", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "node")))
                          "not all elements are node objects"
                       else
                         TRUE)