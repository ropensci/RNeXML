#' Strucrure of meta S4 class.
#'
#' Class that represent simple annotation structure
#'
#' \describe{
#'    \item{literal}{A literal  note value}
#'
#'  }

require("XML")

#' @name annotation-class
#' @rdname annotation-class
#' @exportClass annotation

setClass("annotation",
    representation(literal = "character")
)

## Get annotation value
setMethod(f="getAnnotation", signature="annotation", definition=function(object)
{
    value <- object@literal
    return(value)
})

## Set annotation value
setMethod(f="setAnnotation", signature="annotation", definition=function(object, value)
{
    object@literal<- value
})