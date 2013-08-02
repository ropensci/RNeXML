#' Strucrure of tree S4 class.
#'
#' Class will be hold information about tree parsed from NeXML file in DOM mode.
#'
#' \describe{
#'    \item{id}{An identification value of tree}
#'
#'    \item{xsi:type}{Datatype value}
#'
#'    \item{label}{Label value}
#'
#'    \item{nodes}{Set of node items}
#'
#'    \item{edges}{Set of edge items}
#'
#'  }
#' @name tree-class
#' @rdname tree-class
#' @exportClass tree

setClass("tree",
         representation(id         = "character", 
                        'xsi:type' = "character", 
                        label      = "character", 
                        nodes       = "ListOfNode", 
                        edges       = "ListOfEdge",
                        meta       = "ListOfMeta")
)