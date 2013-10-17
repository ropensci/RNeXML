


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
            if(!is.na(xmlAttrs(from)["type"]))  ## FIXME use [["type"]] or ["type"]
                slot(obj, "xsi:type") <- xmlAttrs(from)["type"]
            if(!is.na(xmlAttrs(from)["xsi:type"])) ## Shouldn't be necessary but seems to be for first test in test_inheritance.R...
                slot(obj, "xsi:type") <- xmlAttrs(from)["xsi:type"]
               obj
          }
)

#########################

setClass("Meta",
         representation(children = "list"), 
         contains = "Base")
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
         representation(id = "character",
                        property = "character", 
                        datatype = "character",
                        content = "character"),
         contains="Meta")
setMethod("fromNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            obj@property <- attrs[["property"]]
            if(!is.na(attrs["datatype"]))
                 obj@datatype <- attrs[["datatype"]]
            if(!is.na(attrs["content"]))
                 obj@content <- attrs[["content"]]
            if(!is.na(attrs["id"]))
                 obj@id <- attrs[["id"]]
               obj
          }
)
setMethod("toNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            attrs <- c(id = unname(object@id),
                       property = unname(object@property),  # required
                       datatype = unname(object@datatype),  # optional
                       content = unname(object@content))   # required
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
})
setAs("XMLInternalElementNode", "LiteralMeta", function(from) fromNeXML(new("LiteralMeta"), from)) 
setAs("LiteralMeta", "XMLInternalElementNode", function(from) toNeXML(from, newXMLNode("meta")))
setAs("LiteralMeta", "XMLInternalNode", function(from) toNeXML(from, newXMLNode("meta")))


##############################################

setClass("ResourceMeta", 
         representation(id = "character",
                        rel = "character", 
                        href = "character"),
         contains="Meta")
setMethod("fromNeXML", 
          signature("ResourceMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            obj@href <- attrs[["href"]]
            if(!is.na(attrs[["rel"]]))
                 obj@rel <- attrs[["rel"]]
               obj
          }
)
setMethod("toNeXML", 
          signature("ResourceMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            attrs <- c(id = unname(object@id),
                       href = unname(object@href),
                       rel = unname(object@rel))  
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
})
setAs("XMLInternalElementNode", "ResourceMeta", function(from) fromNeXML(new("ResourceMeta"), from)) 
setAs("ResourceMeta", "XMLInternalElementNode", function(from) toNeXML(from, newXMLNode("meta")))
setAs("ResourceMeta", "XMLInternalNode", function(from) toNeXML(from, newXMLNode("meta")))



##############################################

setClass("meta", 
         contains=c("LiteralMeta", "ResourceMeta"))
setAs("XMLInternalElementNode", "meta", function(from){ 
      type <- xmlAttrs(from)["type"]
      if(is.na(type)) ## FIXME This is CRUDE
        type <- xmlAttrs(from)["xsi:type"]
      if(is.na(type)) # if still not defined...
        fromNeXML(new("meta", from))
      else {
        type <- gsub("nex:", "", type) ## FIXME This is CRUDE
        fromNeXML(new(type[1]), from)
      }
})

setAs("meta", "XMLInternalElementNode", function(from){ 
      toNeXML(as(from, slot(from, "xsi:type")), newXMLNode("meta", .children = from@children))
})
setAs("meta", "XMLInternalNode", function(from)  ## Really, do we need this?
      toNeXML(as(from, slot(from, "xsi:type")), newXMLNode("meta", .children = from@children)))
# Methods inherited automatically?



###############################################

setClass("ListOfmeta", contains = "list")
      

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
             if(length(object@about) > 0)
               addAttributes(parent, "about" = object@about)
             parent
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
          signature("OptionalTaxonLinked", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@otu) > 0)
               addAttributes(parent, "otu" = object@otu)
            parent
          })

##############################


setClass("TaxaLinked", 
         representation(otus = "character"),
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("TaxaLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otus"]))
               obj@otus <- xmlAttrs(from)["otus"]       
             obj
          }
)
setMethod("toNeXML", 
          signature("TaxaLinked", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@otus) > 0)
               addAttributes(parent, "otus" = object@otus)
            parent
          })


############################## Really AbstractNode

setClass("node", 
         representation(root = "logical"),
         contains = "OptionalTaxonLinked")
setMethod("fromNeXML", signature("node", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod() 
             if(!is.na(xmlAttrs(from)["root"]))
               obj@root <- as.logical(xmlAttrs(from)["root"])
             obj
          }
)
setMethod("toNeXML", 
          signature("node", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@root) > 0)
               addAttributes(parent, "root" = tolower(object@root))
            parent
          })
setAs("node", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("node")))
setAs("node", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("node")))
setAs("XMLInternalElementNode", "node",
      function(from) fromNeXML(new("node"), from))



################################ Really AbstractEdge

setClass("edge", 
         representation(source = "character",
                        target = "character", 
                        length = "numeric"), 
         contains="IDTagged")
setMethod("fromNeXML", 
          signature("edge", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            obj@source <- attrs["source"]
            obj@target <- attrs["target"]
             if(!is.na(attrs["length"]))
               obj@length <- as.numeric(attrs["length"])
             obj
          }
)
setMethod("toNeXML", 
          signature("edge", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "source" = object@source)
            addAttributes(parent, "target" = object@target)
            if(length(object@length) > 0)
               addAttributes(parent, "length" = object@length)
            parent
          })
setAs("edge", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("edge")))
setAs("edge", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("edge")))
setAs("XMLInternalElementNode", "edge",
      function(from) fromNeXML(new("edge"), from))


##################################################

setClass("rootEdge", 
         representation(source = "character",
                        target = "character", 
                        length = "numeric"), 
         contains="IDTagged")
setMethod("fromNeXML", 
          signature("rootEdge", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            obj@target <- attrs["target"]
             if(!is.na(attrs["length"]))
               obj@length <- as.numeric(attrs["length"])
             obj
          }
)
setMethod("toNeXML", 
          signature("rootEdge", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "target" = object@target)
            if(length(object@length) > 0)
               addAttributes(parent, "length" = object@length)
            parent
          })
setAs("rootEdge", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("rootedge"))) 
setAs("rootEdge", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("rootedge"))) 
setAs("XMLInternalElementNode", "rootEdge",
      function(from) fromNeXML(new("rootEdge"), from))


################################ alternatively called "Taxon" by the schema


setClass("otu", contains = "IDTagged")
setMethod("fromNeXML", 
          signature("otu", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj
          })
setMethod("toNeXML", 
          signature("otu", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            parent
          })
setAs("otu", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("otu")))
setAs("otu", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("otu")))
setAs("XMLInternalElementNode", "otu",
      function(from) fromNeXML(new("otu"), from))

setAs("otu", "XMLInternalNode",
           function(from) toNeXML(from, newXMLNode("otu")))
setAs("otu", "XMLInternalElementNode",
           function(from) toNeXML(from, newXMLNode("otu")))

################################ alternatively called Taxa by the schema

setClass("ListOfotu", 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "otu")))
                          "not all elements are otu objects"
                       else
                         TRUE)

###############################

setClass("otus", 
         representation(otu = "ListOfotu"), 
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("otus", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@otu <- new("ListOfotu", 
                              lapply(kids[names(kids) == "otu"], 
                                     as, "otu"))
            obj
          })
setMethod("toNeXML", 
          signature("otus", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@otu)
            parent
          })
setAs("otus", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("otus")))
setAs("otus", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("otus")))
setAs("XMLInternalElementNode", "otus",
      function(from) fromNeXML(new("otus"), from))

################################


setClass("ListOfedge", 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "edge")))
                          "not all elements are meta objects"
                       else
                         TRUE)



setClass("ListOfnode", 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "node")))
                          "not all elements are meta objects"
                       else
                         TRUE)

################################## actually AbstractTree

setClass("tree", 
         representation(node = "ListOfnode", 
                        edge = "ListOfedge",
                        rootedge = "rootEdge"), # Actually AbstractRootEdge
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("tree", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@node <- new("ListOfnode", 
                            lapply(kids[names(kids) == "node"], 
                                   as, "node"))
            obj@edge <- new("ListOfedge", 
                            lapply(kids[names(kids) == "edge"], 
                                   as, "edge"))
            obj
          })
setMethod("toNeXML", 
          signature("tree", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@node)
            addChildren(parent, kids = object@edge)
            parent
          })
setAs("tree", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("tree")))
setAs("tree", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("tree")))
setAs("XMLInternalElementNode", "tree",
      function(from) fromNeXML(new("tree"), from))



################################################

setClass("ListOftree", contains = "list") # validity can contain tree or network nodes?

setClass("trees", 
         representation(tree = "ListOftree"), # Can contain networks...
         contains = "TaxaLinked")
setMethod("fromNeXML", 
          signature("trees", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@tree <- new("ListOftree", 
                            lapply(kids[names(kids) == "tree"], 
                                   as, "tree"))
            obj
          })
setMethod("toNeXML", 
          signature("trees", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@tree)
#            addChildren(parent, kids = object@network)
            parent
          })
setAs("trees", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("trees")))
setAs("trees", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("trees")))
setAs("XMLInternalElementNode", "trees",
      function(from) fromNeXML(new("trees"), from))


####################################################

setClass("ListOftrees", contains = "list")

####################################################



nexml_namespaces <- 
  c("nex"   = "http://www.nexml.org/2009",
    "xsi"   = "http://www.w3.org/2001/XMLSchema-instance",
    "xml"   = "http://www.w3.org/XML/1998/namespace",
    "cdao"  = "http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#",
    "xsd"   = "http://www.w3.org/2001/XMLSchema#",
    "dc"    = "http://purl.org/dc/elements/1.1/",
  "dcterms" = "http://purl.org/dc/terms/",
    "prism" = "http://prismstandard.org/namespaces/1.2/basic/",
    "cc"    = "http://creativecommons.org/ns#",
    "ncbi"  = "http://www.ncbi.nlm.nih.gov/taxonomy#",
    "tc"    = "http://rs.tdwg.org/ontology/voc/TaxonConcept#")


setClass("nexml", 
         representation(version = "character",
                        generator = "character",
                        "xsi:schemaLocation" = "character", # part of base?
                        namespaces = "character",           # part of base? 
                        otus = "otus",
                        trees = "ListOftrees"), 
         prototype = prototype(version = "0.9",
                   generator = "RNeXML",
                   "xsi:schemaLocation" = "http://www.nexml.org/2009/nexml.xsd",
                   namespaces = c(nexml_namespaces, 
                                  "http://www.nexml.org/2009")),
         contains = "Annotated")

setMethod("fromNeXML", 
          signature("nexml", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()

            # handle attributes
            attrs <- xmlAttrs(from)
            obj@version <- attrs["version"]     # required attribute
            if(!is.na(attrs["generator"]))       # optional attribute
               obj@generator <- attrs["generator"]

            # handle these guys, kinda attributes? Are they optional?
            slot(obj, "xsi:schemaLocation") <- attrs["xsi:schemaLocation"]

            # Handle children
            kids <- xmlChildren(from)
            obj@otus <- as(from[["otus"]], "otus")
            obj@trees <- new("ListOftrees", 
                            lapply(kids[names(kids) == "trees"], 
                                   as, "trees"))
            obj
          })

setMethod("toNeXML", 
          signature("nexml", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "version" = object@version)
            if(length(object@generator)>0)
              addAttributes(parent, "generator" = object@generator)

            # Coercion of object to XML happens automatically
            addChildren(parent, object@otus) # a single "otus" object
            addChildren(parent, kids = object@trees) # a list of "trees" objects
            parent
          })
setAs("nexml", "XMLInternalNode",
      function(from) suppressWarnings(toNeXML(from, newXMLNode("nex:nexml", namespaceDefinitions = from@namespaces))))

setAs("nexml", "XMLInternalElementNode",
      function(from) suppressWarnings(toNeXML(from, newXMLNode("nex:nexml", namespaceDefinitions = from@namespaces))))
setAs("XMLInternalElementNode", "nexml",
      function(from) fromNeXML(new("nexml"), from))



#######################################################


