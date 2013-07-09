#' Strucrure of NeXML.Metadata S4 class.
#'
#' Class will be hold information about metadata of some node parsed from NeXML tree in DOM mode.
#'
#' \describe{
#'    \item{hash}{A logical keeping of metadata as a dictionary.}
#'
#'  }

require("hash")

#' @name NeXML.Metadata-class
#' @rdname NeXML.Metadata-class
#' @exportClass NeXML.Metadata

setClass("NeXML.Metadata",
    representation(mdata="hash")
)