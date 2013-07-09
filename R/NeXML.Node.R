#' Strucrure of NeXML.Node S4 class.
#'
#' Class will be hold information about node from NeXML tree in DOM mode.
#'
#' \describe{
#'    \item{type}{A logical keeping track of something.}
#'
#'    \item{mdata}{An integer specifying something else.}
#' 
#'    \item{parentNode}{A data.frame holding some data.}
#' 
#'    \item{childNodes}{A data.frame holding data about child node.}
#'  }
#' @name NeXML.Node-class
#' @rdname NeXML.Node-class
#' @exportClass NeXML.Node
setClass("NeXML.Node",
    representation(name="character",
        mdata="list"
        parent="Node",
        children="list",
        value="character")
)

#' validate input NeXML data
setValidity("NeXML.Node", function(object){
		obj.name <- object@name
		obj.mdata <- object@mdata
		obj.parent <- object@parent
		obj.children <- object@children
		obj.value <- object@value

		if(isTRUE(!nchar(obj.name) > 0) & isTRUE(!nchar(obj.value) > 0)){
			print(str(object))
			stop(simpleError("Invalid object: A node must at least have a name or a value!"))
		} else {}

		obj.mdata.names <- names(obj.mdata)

		# if there are attributes, check that they all have names
		if(length(obj.mdata) > 0){
			if(length(obj.mdata) != length(obj.mdata.names)){
				stop(simpleError("Invalid object: All attributes must have names!"))
			} else {}
		} else {}

		# check content of children
		if(length(obj.children) > 0){
			child.nodes <- sapply(obj.children, function(this.child){is.NeXML.Node(this.child)})
			if(!all(child.nodes)){
				stop(simpleError("Invalid object: All list elements of children must be of class NeXML.Node!"))
			} else {}
		} else {}
	return(TRUE)
})
