#' Strucrure of meta S4 class.
#'
#' Class will be hold information about metadata of some node parsed from NeXML tree in DOM mode.
#'
#' \describe{
#'    \item{source}{Value of edge class}
#'
#'    \item{target}{Target}
#'
#'    \item{id}{An identifying number of edge instance}
#'
#'    \item{length}{Edge lenght}
#'
#'  }
#' @name edge-class
#' @rdname edge-class
#' @exportClass edge

setClass("edge",
    representation(source = "character",
                   target = "character",
                   id     = "character",
                   length = "numeric")
)