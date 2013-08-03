setClass("meta", 
          representation(id       = "character", 
                         property = "character", 
                         content  = "character", 
                         type     = "character", 
                         datatype = "character"))

setClass("node",
    representation(id    = "character",
                   label = "character",
                   otu   = "character",
                   about = "character",
                   root  = "logical",
                   meta = "meta"))
setClass("edge",
    representation(source = "character",
                   target = "character",
                   id     = "character",
                   length = "numeric",
                   meta   = "meta"))

setClass("ListOfnode", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "node")))
                          "not all elements are node objects"
                       else
                         TRUE)

setClass("ListOfedge", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "edge")))
                          "not all elements are edge objects"
                       else
                         TRUE)

setClass("tree",
         representation(id         = "character", 
                        'xsi:type' = "character", 
                        label      = "character", 
                        nodes       = "ListOfnode", 
                        edges       = "ListOfedge"))


setAs("XMLInternalElementNode", "node", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "meta", function(from) xmlToS4(from))
setAs("XMLInternalElementNode", "edge", function(from) xmlToS4(from))

setAs("XMLInternalElementNode", "tree",
       function(from) {
         obj = new("tree")
         kids = xmlChildren(from)
         obj@edges = new("ListOfedge", lapply(kids[names(kids) == "edge"], as, "edge"))
         obj@nodes = new("ListOfnode", lapply(kids[names(kids) == "node"], as, "edge"))
         ats = xmlAttrs(from, addNamespacePrefix = TRUE)
         for(i in names(ats))
            slot(obj, i) = ats[i]
         obj
       })


################################################ Going back from S4 to XML ####################

setAs("tree", "XMLInternalElementNode",
      function(from){
        tree <- from
        obj <- newXMLNode("tree", attrs = c(id = tree@id, label = tree@label, 'xsi:type' = slot(tree, 'xsi:type'))) 
        addChildren(obj, kids = tree@nodes) # kids envokes coercion 
        addChildren(obj, kids = tree@edges) # kids envokes coercion 
      })

setAs("edge", "XMLInternalNode", 
      function(from){
         x <- from
         
         ## Avoid returning empty nodes -- is this necessary?
         if(is_class_empty(x@meta))
          kids <- list()
         else
          kids <- x@meta


         newXMLNode("edge", attrs = c(source = x@source, target = x@target, id = x@id, length = x@length), kids = kids)
      })

setAs("node", "XMLInternalNode", 
      function(from){
         x <- from
         
         ## Avoid returning empty nodes -- is this necessary?
         if(is_class_empty(x@meta))
          kids <- list()
         else
          kids <- x@meta


         newXMLNode("node", attrs = c(x@id, x@label, x@otu), kids = kids)
      })

setAs("meta", "XMLInternalNode",
      function(from){
        x <- from
        attrs = c(id = x@id, property = x@property, content = x@content, type = x@type,  datatype = x@datatype)
        newXMLNode("meta", attrs = attrs)
      })




is_class_empty <- function(x){
  nms <- slotNames(x)
  !as.logical(sum(sapply(sapply(nms, slot, object = x), length)))
}
