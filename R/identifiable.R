#' Strucrure of meta S4 class.
#'
#' Class will be hold id of each element, so this element can be presented as identifiable.
#'
#' \describe{
#'    \item{id}{An identifying value of node instance}
#'
#'  }
#' @name identifiable-class
#' @rdname identifiable-class
#' @exportClass identifiable

setClass("identifiable",
    representation(id = "character")
)

## Id getter
#setMethod(f="getId", signature="identifiable", definition=function(object)
#{
#    value <- object@id
#    return(value)
#})

## Id setter
#setMethod(f="setId", signature="identifiable", definition=function(object, value)
#{
#    object@id <- value
#})