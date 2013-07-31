#' Strucrure of meta S4 class.
#'
#' Class that represent annotation for NeXML node
#'
#' \describe{
#'    \item{annotations}{A list of annotations that represent node}
#'
#'  }

#' @name annotatable-class
#' @rdname annotatable-class
#' @exportClass annotatable

setClass("annotatable",
    representation(annotations = "list")
)

## Add annotation
#setMethod(f="addAnnotation", signature="annotatable", definition=function(object, newAnnot)
#{  
#    object@annotations <- c(object@annotations, newAnnot) 
#})

## Get annotation by number
#setMethod(f="getAnnotation", signature="annotatable", definition=function(object, number)
#{
#    return object@annotations[number]
#})

## Get annotation by name
#setMethod(f="getAnnotationByName", signature="annotatable", definition=function(object, name)
#{
#    value <- get(name, object@annotations)
#    return value
#})