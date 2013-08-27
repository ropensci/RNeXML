setGeneric("fromNeXML", function(obj, from) standardGeneric("fromNeXML")) 


setGeneric("toNeXML", 
           valueClass="XMLInternalElementNode",
           function(object, parent) 
             standardGeneric("toNeXML"))

# Rather verbose methods definitions manually reading and writing each class...
# Rather than just slot matching names, we explicitly map each attribute...

##############################

setClass("Base",
         representation('xsi:type' = "character"))
setMethod("toNeXML", 
          signature("Base", "XMLInternalElementNode"), 
          function(object, parent){
            type <- slot(object, "xsi:type")
            if(length(type) > 0)
              addAttributes(parent, 
                           "xsi:type" = type, 
                            suppressNamespaceWarning=TRUE)
            parent
          })
setMethod("fromNeXML", 
          signature("Base", "XMLInternalElementNode"),
          function(obj, from){
            if(!is.na(xmlAttrs(from)["xsi:type"]))
                 slot(obj, "xsi:type") <- xmlAttrs(from)["xsi:type"]
               obj
          }
)

#########################

setClass("Meta", contains = "Base")
setMethod("fromNeXML", 
          signature("Meta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
          }
)
setMethod("toNeXML", 
          signature("Meta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
})


#########################

setClass("LiteralMeta", 
         representation(property = "character", #actually xs:QName
                        datatype = "character",
                        content = "character"),
         contains="Meta")
setMethod("fromNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            obj@property <- attrs["property"]
            if(!is.na(attrs["datatype"]))
                 obj@label <- attrs["datatype"]
            if(!is.na(attrs["content"]))
                 obj@label <- attrs["content"]
               obj
          }
)
setMethod("toNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            attrs <- c(object@property,  # required
                       object@datatype,  # optional
                       object@content)   # required
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
})
setAs("XMLInternalElementNode", "LiteralMeta", function(from) fromNeXML(new("LiteralMeta"), from)) 
setAs("LiteralMeta", "XMLInternalElementNode", function(from) toNeXML(from, newXMLNode("meta")))


##############################################

setClass("meta", contains="LiteralMeta")
setAs("XMLInternalElementNode", "meta", function(from) xmlToS4(from))
setAs("meta", "XMLInternalElementNode", function(from) 
      toNeXML(as(from, "LiteralMeta"), newXMLNode("meta")))
setAs("meta", "XMLInternalNode", function(from)  ## Really, do we need this?
      toNeXML(as(from, "LiteralMeta"), newXMLNode("meta")))
# Methods inherited automatically?



###############################################

setClass("ListOfmeta", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "meta")))
                          "not all elements are meta objects"
                       else
                         TRUE)
#setAs("XMLInternalElementNode", "ListOfmeta", 
#        function(from) list(as(from, "meta")))
      

###############################################


setClass("Annotated",
         representation(meta = "ListOfmeta",
                        about = "character"),
         contains = "Base")
setMethod("fromNeXML",
          signature("Annotated", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@meta <- new("ListOfmeta", 
                              lapply(kids[names(kids) == "meta"], 
                                     as, "meta"))
            if(!is.na(xmlAttrs(from)["about"]))
               obj@about <- xmlAttrs(from)["about"]
            obj
})
setMethod("toNeXML", 
          signature("Annotated", "XMLInternalElementNode"), 
           function(object, parent){
             parent <- callNextMethod()
             addChildren(parent, kids = object@meta)
             addAttributes(parent, 
                           "about" = slot(object, "about"), 
                           suppressNamespaceWarning=TRUE)
          })


######################################################

setClass("Labelled",
         representation(label = "character"),
         contains = "Annotated")
setMethod("fromNeXML", 
          signature("Labelled", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            if(!is.na(xmlAttrs(from)["label"]))
                 obj@label <- xmlAttrs(from)["label"]
               obj
          }
)
setMethod("toNeXML", 
          signature("Labelled", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@label) > 0)
               addAttributes(parent, "label" = object@label)
            parent
          })

##############################

setClass("IDTagged",
         representation(id = "character"),
         contains = "Labelled")
setMethod("fromNeXML", 
          signature("IDTagged", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            if(!is.na(xmlAttrs(from)["id"]))
                 obj@id <- xmlAttrs(from)["id"]
               obj
          }
)
setMethod("toNeXML", 
          signature("IDTagged", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@id) > 0)
               addAttributes(parent, "id" = object@id)
            parent
          })


##############################


setClass("OptionalTaxonLinked", 
         representation(otu = "character"),
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("OptionalTaxonLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otu"]))
               obj@otu <- xmlAttrs(from)["otu"]       
             obj
          }
)
setMethod("toNeXML", 
          signature("IDTagged", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@otu) > 0)
               addAttributes(parent, "otu" = object@otu)
            parent
          })

##############################

setClass("node", 
         representation(root = "logical"),
         contains = "OptionalTaxonLinked")
setMethod("fromNeXML", signature("node", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod() 
             if(!is.na(xmlAttrs(from)["root"]))
               obj@root <- xmlAttrs(from)["root"]
             obj
          }
)
setMethod("toNeXML", 
          signature("node", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@root) > 0)
               addAttributes(parent, "root" = object@root)
            parent
          })
setAs("node", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("node")))
setAs("XMLInternalElementNode", "node",
      function(from) fromNeXML(new("node"), from))

