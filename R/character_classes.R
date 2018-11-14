#' @include classes.R

####################################################


setClass("nexml:char",
         slots = c(states = "character"),
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:char", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            if(!is.na(xmlAttrs(from)["states"]))
              obj@states <- xmlAttrs(from)["states"]
            obj
          })
setMethod("toNeXML", 
          signature("nexml:char", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@states) > 0)
              addAttributes(parent, "states" = object@states)
            parent
          })
setAs("nexml:char", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:char", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:char",
      function(from) fromNeXML(new("nexml:char"), from))




###############################################

setClass("ListOfrow", slots = c(names="character"), contains="list")
setClass("nexml:matrix",
         slots = c(row="ListOfrow"),
         contains = "nexml:Annotated")
setMethod("fromNeXML", 
          signature("nexml:matrix", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@row <- new("ListOfrow", 
                              lapply(kids[names(kids) == "row"], 
                                     as, "nexml:row"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:matrix", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@row)
            parent
          })
setAs("nexml:matrix", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:matrix", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:matrix",
      function(from) fromNeXML(new("nexml:matrix"), from))





######################################################

setClass("ListOfcell", slots = c(names="character"), contains="list")
setClass("ListOfseq", slots = c(names="character"), contains="list")

setClass("nexml:row",
         slots = c(cell = "ListOfcell",
                        seq = "ListOfseq"),
         contains = "nexml:OptionalTaxonLinked")
setMethod("fromNeXML", 
          signature("nexml:row", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0){
              if("cell" %in% names(kids))
              obj@cell <- new("ListOfcell", 
                lapply(kids[names(kids) == "cell"], as, "nexml:cell"))
              if("seq" %in% names(kids))
              obj@seq <- new("ListOfseq", 
                lapply(kids[names(kids) == "seq"], as, "nexml:seq"))
            }
            obj
          })
setMethod("toNeXML", 
          signature("nexml:row", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@cell)
            addChildren(parent, kids = object@seq)
            parent
          })
setAs("nexml:row", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:row", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:row",
      function(from) fromNeXML(new("nexml:row"), from))

#######################################################
setClass("ListOfstate", slots = c(names="character"), contains="list")
setClass("ListOfpolymorphic_state_set", slots = c(names="character"), contains="list")
setClass("ListOfuncertain_state_set", slots = c(names="character"), contains="list")

setClass("nexml:states",
         slots = c(state="ListOfstate", 
                   polymorphic_state_set="ListOfpolymorphic_state_set",
                   uncertain_state_set="ListOfuncertain_state_set"),
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:states", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0){
              obj@state <- new("ListOfstate", 
                              lapply(kids[names(kids) == "state"], 
                                     as, "nexml:state"))
              obj@polymorphic_state_set <- new("ListOfpolymorphic_state_set", 
                             lapply(kids[names(kids) == "polymorphic_state_set"], 
                                    as, "nexml:polymorphic_state_set"))
              obj@uncertain_state_set <- new("ListOfuncertain_state_set", 
                                               lapply(kids[names(kids) == "uncertain_state_set"], 
                                                      as, "nexml:uncertain_state_set"))
            }
            obj
          })
setMethod("toNeXML", 
          signature("nexml:states", "XMLInternalElementNode"),
          function(object, parent){
            suppressWarnings({ # avoid arcane XML warning message
            parent <- callNextMethod()
            addChildren(parent, kids = object@state)
            addChildren(parent, kids = object@uncertain_state_set)
            addChildren(parent, kids = object@polymorphic_state_set)
            })
            parent
          })
setAs("nexml:states", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:states", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:states",
      function(from) fromNeXML(new("nexml:states"), from))




####################################################### 
## technically symbol is positive integer http://nexml.org/doc/schema-1/characters/standard/#StandardToken
setClass("nexml:state",
         slots = c(symbol = "integer"), 
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:state", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@symbol <- as.integer(xmlAttrs(from)["symbol"])
            obj
          })
setMethod("toNeXML", 
          signature("nexml:state", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "symbol" = object@symbol)
            parent
          })
setAs("nexml:state", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:state", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:state",
      function(from) suppressWarnings(fromNeXML(new("nexml:state"), from)))

################################################

##  a symbol for an uncertain stat is a character string
setClass("nexml:uncertain_state",
         slots = c(symbol = "character"), 
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:uncertain_state", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@symbol <- xmlAttrs(from)["symbol"]
            obj
          })
setMethod("toNeXML", 
          signature("nexml:uncertain_state", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "symbol" = object@symbol)
            parent
          })
setAs("nexml:uncertain_state", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:uncertain_state", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:uncertain_state",
      function(from) suppressWarnings(fromNeXML(new("nexml:uncertain_state"), from)))

################################################


setClass("ListOfmember", slots = c(names="character"), contains="list")

setClass("nexml:uncertain_state_set", 
         slots = c(member = "ListOfmember"),
         contains="nexml:uncertain_state")
setMethod("fromNeXML", 
          signature("nexml:uncertain_state_set", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@member <- new("ListOfmember", 
                              lapply(kids[names(kids) == "member"], 
                                     as, "nexml:member"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:uncertain_state_set", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@member)
            parent
          })
setAs("nexml:uncertain_state_set", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:uncertain_state_set", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:uncertain_state_set",
      function(from) fromNeXML(new("nexml:uncertain_state_set"), from))

################################################

setClass("nexml:polymorphic_state_set", contains="nexml:uncertain_state_set")
setMethod("fromNeXML", 
          signature("nexml:polymorphic_state_set", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@member <- new("ListOfmember", 
                              lapply(kids[names(kids) == "member"], 
                                     as, "nexml:member"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:polymorphic_state_set", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@member)
            parent
          })
setAs("nexml:polymorphic_state_set", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:polymorphic_state_set", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:polymorphic_state_set",
      function(from) fromNeXML(new("nexml:polymorphic_state_set"), from))


#####################

setClass("nexml:cell",
         slots = c(char="character", 
                        state= "character"),
         contains="nexml:Labelled")
setMethod("fromNeXML", 
          signature("nexml:cell", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@char <- xmlAttrs(from)["char"]
            obj@state <- xmlAttrs(from)["state"]
            obj
          })
setMethod("toNeXML", 
          signature("nexml:cell", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "char" = object@char)
            addAttributes(parent, "state" = object@state)
            parent
          })
setAs("nexml:cell", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:cell", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:cell",
      function(from) fromNeXML(new("nexml:cell"), from))

#########################

setClass("nexml:member", 
         slots = c(state="character"),
         contains="nexml:Base")
setMethod("fromNeXML", 
          signature("nexml:member", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@state <- xmlAttrs(from)["state"]
            obj
          })
setMethod("toNeXML", 
          signature("nexml:member", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "state" = object@state)
            parent
          })
setAs("nexml:member", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:member", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:member",
      function(from) fromNeXML(new("nexml:member"), from))


########################

setClass("nexml:seq", 
         slots = c(seq = "character"),
         contains="nexml:Base")
setMethod("fromNeXML", 
          signature("nexml:seq", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@seq <- xmlValue(from)
            obj
          }
)
setMethod("toNeXML", 
          signature("nexml:seq", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, object@seq)
            parent
          })
setAs("nexml:seq", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:seq", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:seq",
      function(from) fromNeXML(new("nexml:seq"), from))

#########################################

setClass("ListOfchar", slots = c(names="character"), contains="list")
setClass("ListOfstates", slots = c(names="character"), contains="list")

setClass("nexml:format", 
         slots = c(states = "ListOfstates", ## FIXME Should be ListOfstates
                        char = "ListOfchar"),
         contains = "nexml:Annotated")
setMethod("fromNeXML", 
          signature("nexml:format", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0){
              if("char" %in% names(kids))
                obj@char <- new("ListOfchar", 
                                lapply(kids[names(kids) == "char"], 
                                       as, "nexml:char"))
              if("states" %in% names(kids))
                obj@states <- new("ListOfstates", 
                                lapply(kids[names(kids) == "states"], 
                                       as, "nexml:states"))
            }
            obj
          })
setMethod("toNeXML", 
          signature("nexml:format", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(!isEmpty(object@char))
              addChildren(parent, kids = object@char)
            if(length(object@states) > 0)
              addChildren(parent, kids = object@states)
            parent
          })
setAs("nexml:format", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:format", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:format",
      function(from) fromNeXML(new("nexml:format"), from))





####################################################
setClass("nexml:characters",
         slots = c(format = "nexml:format",
                        matrix = "nexml:matrix"),
        contains = "nexml:TaxaLinked")
setMethod("fromNeXML", 
          signature("nexml:characters", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj@format <- as(from[["format"]], "nexml:format")
            obj@matrix <- as(from[["matrix"]], "nexml:matrix")
            obj
          })
setMethod("toNeXML", 
          signature("nexml:characters", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            parent <- addChildren(parent, format = object@format)
            parent <- addChildren(parent, matrix = object@matrix)
            parent
          })
setAs("nexml:characters", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:characters", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:characters",
      function(from) fromNeXML(new("nexml:characters"), from))


