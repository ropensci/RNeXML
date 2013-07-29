#' Strucrure of node S4 class.
#'
#' Class will be hold information about node from NeXML tree in DOM mode.
#'
#' \describe{
#'    \item{id}{An identifying number of node.}
#'
#'    \item{label}{Name of the node (tree, otu, etc).}
#' 
#'    \item{otu}{Value of the 'otu' attribute}
#
#'    \item{about}{Node description}
#' 
#'    \item{meta}{Metadata that is presented by meta class.}
#'  }
#' @name node-class
#' @rdname node-class
#' @exportClass node

setClass("node",
    representation(id="character",
        label = "character"
        otu = "character"
        about = "character"
        meta="meta")
)

#' validate input NeXML data
#setValidity("node", function(object){
#		obj.id <- object@id
#		obj.label <- object@label
#		obj.otu <- object@otu
#		obj.about <- object@about
#		obj.meta <- object@meta
#
#		if(isTRUE(!nchar(obj.id) > 0) & isTRUE(!nchar(obj.otu) > 0)){
#			print(str(object))
#			stop(simpleError("Invalid object: A node must at least have an id or a value!"))
#		} else {}
#
#		meta <- object@meta
#		obj.meta.id <- meta@id
#		obj.meta.content <- meta@content
#
#		# if there are attributes, check that they all have names
#		if(!length(obj.meta.content) > 0){
#			stop(simpleError("Invalid object: Meta must have content!"))
#
#		} else {}
#
#		# check meta of node
#		if(isTRUE(!nchar(obj.meta.id) > 0) || isTRUE(!nchar(obj.meta.datatype) > 0)){
#			print(str(object))
#			stop(simpleError("Invalid object: Meta must have at least have an id or a datatype!"))
#		} else {}
#
#	return(TRUE)
#})
