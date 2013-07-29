#' Strucrure of meta S4 class.
#'
#' Class will be hold information about metadata of some node parsed from NeXML tree in DOM mode.
#'
#' \describe{
#'    \item{id}{An identifyong number of meta instance.}
#'
#'    \item{property}{A propetry of metadata.}
#'
#'    \item{content}{Content of metadata class.}
#'
#'    \item{datatype}{Type of presented metadata.}
#'
#'  }

require("XML")

#' @name meta-class
#' @rdname meta-class
#' @exportClass meta

setClass("meta",
    representation(id="character",
    property="character",
    content="logical",
    'xsi:type'="character",
    datatype="character")
)

#setAs("XMLInternalElementNode", "meta", function(from) xmlToS4(from))