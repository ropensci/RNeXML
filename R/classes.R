


setGeneric("fromNeXML", function(obj, from) standardGeneric("fromNeXML")) 


setGeneric("toNeXML", 
           valueClass="XMLInternalElementNode",
           function(object, parent) 
             standardGeneric("toNeXML"))

# Rather verbose methods definitions manually reading and writing each class...
# Rather than just slot matching names, we explicitly map each attribute...

##############################

setClass("nexml:Base",
         slots = c('xsi:type' = "character",
                   # in the NeXML schema, the Base type already allows for xml:id
                   'id' = "character",
                   'nexml:xmlName' = "character",
                   'nexml:idrefName' = "character"))
setMethod("initialize",
          signature("nexml:Base"),
          function(.Object, ...) {
            obj <- callNextMethod(.Object, ...)
            slot(obj, "nexml:xmlName") <- tolower(sub("nexml:", "", class(obj),
                                                      fixed = TRUE))
            slot(obj, "nexml:idrefName") <- slot(obj, "nexml:xmlName")
            obj
          })
setMethod("toNeXML", 
          signature("nexml:Base", "XMLInternalElementNode"), 
          function(object, parent){
            type <- slot(object, "xsi:type")
            if(length(type) > 0){
              #if(is.na(pmatch("nex:", type)))  # nex or relevant namespace should come from default anyway
              #  type <- paste0("nex:", type)
              addAttributes(parent, 
                           "xsi:type" = type,  
                            suppressNamespaceWarning=TRUE) # We always define xsi namespace in the header... 
            }
            if(length(object@id) > 0)
              addAttributes(parent, "id" = object@id)
            parent
          })
setMethod("fromNeXML", 
          signature("nexml:Base", "XMLInternalElementNode"),
          function(obj, from){
            attrs <- xmlAttrs(from)
            if(!is.null(attrs)){
              if(!is.na(attrs["type"]))  ## FIXME use [["type"]] or ["type"]
                slot(obj, "xsi:type") <- as.character(attrs["type"])
              if(!is.na(attrs["xsi:type"])) ## Shouldn't be necessary but seems to be for first test in test_inheritance.R...
                slot(obj, "xsi:type") <- as.character(attrs["xsi:type"])
              if(!is.na(attrs["id"]))
                obj@id <- as.character(attrs["id"])
            }
            obj
          }
)

#########################

setClass("nexml:Meta",
         slots = c(children = "list"), 
         contains = "nexml:Base")
setMethod("initialize",
          signature("nexml:Meta"),
          function(.Object, ...) {
            obj <- callNextMethod(.Object, ...)
            slot(obj, "nexml:xmlName") <- "meta"
            slot(obj, "nexml:idrefName") <- sub("nexml:", "", class(obj), 
                                                fixed = TRUE)
            obj
          })
setMethod("fromNeXML", 
          signature("nexml:Meta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
          }
)
setMethod("toNeXML", 
          signature("nexml:Meta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
})


#########################

setClass("nexml:LiteralMeta", 
         slots = c(property = "character", 
                   datatype = "character",
                   content = "character"),
         contains="nexml:Meta")
setMethod("fromNeXML", 
          signature("nexml:LiteralMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            literalContent <- xmlValue(from)
            if (literalContent != "")
              obj@content <- literalContent
            attrs <- xmlAttrs(from)
            obj@property <- attrs[["property"]]
            if(!is.na(attrs["datatype"]))
                 obj@datatype <- attrs[["datatype"]]
            if(is.na(attrs["content"])) {
                 kids <- xmlChildren(from)
                 if (length(kids) == 1 && names(kids) != "text") {
                   cnt <- saveXML(kids[[1]], indent = FALSE)
                   # need to capture the XML namespaces for later serialization
                   nsList <- xmlNamespace(kids[[1]])
                   attr(cnt, "namespaces") <- nsList
                   if (length(nsList) > 0)
                     # now that namespaces are captured, their declarations
                     # are redundant and can get in the way at the string level
                     cnt <- gsub(" xmlns:[A-Za-z0-9=]*\"[^\"]+\"", "", cnt)
                   obj@content <- xml(cnt)
                 }
            } else
                 obj@content <- attrs[["content"]]
            obj
          }
)
setMethod("toNeXML", 
          signature("nexml:LiteralMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            cnt <- object@content
            if (length(cnt) > 0 && class(cnt) == "XMLString") {
              # need to add back in the namespace definitions used by the
              # XML value to avoid a stream of warnings, even if this will
              # probably be redundant with the doc root
              parseXMLAndAdd(cnt, parent = parent, nsDefs = attr(cnt, "namespaces"))
              cnt <- character(0)
            }
            attrs <- c(property = unname(object@property),  # required
                       datatype = unname(object@datatype),  # optional
                       content = unname(cnt))               # required
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
})
setAs("XMLInternalElementNode", "nexml:LiteralMeta",
      function(from) fromNeXML(new("nexml:LiteralMeta"), from))
setAs("nexml:LiteralMeta", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:LiteralMeta", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))


##############################################

setClass("nexml:ResourceMeta", 
         slots = c(rel = "character", href = "character"),
         contains="nexml:Meta")
setMethod("fromNeXML", 
          signature("nexml:ResourceMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            if(!is.na(attrs["href"]))
              obj@href <- attrs[["href"]]
            if(!is.na(attrs[["rel"]]))
                 obj@rel <- attrs[["rel"]]
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@children <- lapply(kids[names(kids) == "meta"], as, "nexml:meta")
            obj
          }
)
setMethod("toNeXML", 
          signature("nexml:ResourceMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            attrs <- c(href = unname(object@href),
                       rel = unname(object@rel))  
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
            if (length(object@children) > 0)
              addChildren(parent, kids = object@children)
            parent
})
setAs("XMLInternalElementNode", "nexml:ResourceMeta", function(from) fromNeXML(new("nexml:ResourceMeta"), from)) 
setAs("nexml:ResourceMeta", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:ResourceMeta", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))


##############################################

setClass("nexml:meta", 
         contains=c("nexml:LiteralMeta", "nexml:ResourceMeta"))
setAs("XMLInternalElementNode", "nexml:meta", function(from){ 
      type <- xmlAttrs(from)["type"]
      if(is.na(type)) ## FIXME This is CRUDE
        type <- xmlAttrs(from)["xsi:type"]
      if(is.na(type)) # if still not defined...
        fromNeXML(new("nexml:meta", from))
      else {
        type <- paste0("nexml:", gsub(".*:", "", type)) ## FIXME This is CRUDE
        fromNeXML(new(type[1]), from)
      }
})

setAs("nexml:meta", "XMLInternalElementNode", function(from){
      if(length( slot(from, "xsi:type") ) > 0 ){
        if(grepl("LiteralMeta|ResourceMeta", slot(from, "xsi:type")))
          m <- as(from, paste0("nexml:", slot(from, "xsi:type")))
        }
      else
        m <- from
      toNeXML(m, newXMLNode(slot(from, "nexml:xmlName"), .children = from@children))
})
setAs("nexml:meta", "XMLInternalNode", function(from) 
      as(from, "XMLInternalElementNode"))
# Methods inherited automatically?



###############################################

setClass("ListOfmeta", slots = c(names="character"), contains = "list")
      

###############################################


setClass("nexml:Annotated",
         slots = c(meta = "ListOfmeta",
                   about = "character"),
         contains = "nexml:Base")
setMethod("fromNeXML",
          signature("nexml:Annotated", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@meta <- new("ListOfmeta", 
                              lapply(kids[names(kids) == "meta"], 
                                     as, "nexml:meta"))
            if(!is.null(xmlAttrs(from)))
              if(!is.na(xmlAttrs(from)["about"]))
                 obj@about <- xmlAttrs(from)["about"]
            obj
})
setMethod("toNeXML", 
          signature("nexml:Annotated", "XMLInternalElementNode"), 
           function(object, parent){
             parent <- callNextMethod()
             addChildren(parent, kids = object@meta)
             if(length(object@about) > 0)
               addAttributes(parent, "about" = object@about)
             parent
          })


######################################################

setClass("nexml:Labelled",
         slots = c(label = "character"),
         contains = "nexml:Annotated")
setMethod("fromNeXML", 
          signature("nexml:Labelled", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            if(!is.na(xmlAttrs(from)["label"]))
                 obj@label <- xmlAttrs(from)["label"]
               obj
          }
)
setMethod("toNeXML", 
          signature("nexml:Labelled", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@label) > 0)
               addAttributes(parent, "label" = object@label)
            parent
          })

##############################

setClass("nexml:IDTagged", contains = "nexml:Labelled")

##############################


setClass("nexml:OptionalTaxonLinked", 
         slots = c(otu = "character"),
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:OptionalTaxonLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otu"]))
               obj@otu <- as.character(xmlAttrs(from)["otu"])
             obj
          }
)
setMethod("toNeXML", 
          signature("nexml:OptionalTaxonLinked", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@otu) > 0)
               addAttributes(parent, "otu" = object@otu)
            parent
          })

##############################


setClass("nexml:TaxaLinked", 
         slots = c(otus = "character"),
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:TaxaLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otus"]))
               obj@otus <- as.character(xmlAttrs(from)["otus"])
             obj
          }
)
setMethod("toNeXML", 
          signature("nexml:TaxaLinked", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@otus) > 0)
               addAttributes(parent, "otus" = object@otus)
            parent
          })


############################## Really AbstractNode

setClass("nexml:node", 
         slots = c(root = "logical"),
         contains = "nexml:OptionalTaxonLinked")
setMethod("fromNeXML", signature("nexml:node", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod() 
             if(!is.na(xmlAttrs(from)["root"]))
               obj@root <- as.logical(xmlAttrs(from)["root"])
             obj
          }
)
setMethod("toNeXML", 
          signature("nexml:node", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            if(length(object@root) > 0)
               addAttributes(parent, "root" = tolower(object@root))
            parent
          })
setAs("nexml:node", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:node", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:node",
      function(from) fromNeXML(new("nexml:node"), from))



################################ Really AbstractEdge

setClass("nexml:edge", 
         slots = c(source = "character",
                        target = "character", 
                        length = "numeric"), 
         contains="nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:edge", "XMLInternalElementNode"),
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
          signature("nexml:edge", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "source" = object@source)
            addAttributes(parent, "target" = object@target)
            if(length(object@length) > 0)
               addAttributes(parent, "length" = object@length)
            parent
          })
setAs("nexml:edge", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:edge", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:edge",
      function(from) fromNeXML(new("nexml:edge"), from))


##################################################

setClass("nexml:rootEdge", 
         slots = c(source = "character",
                        target = "character", 
                        length = "numeric"), 
         contains="nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:rootEdge", "XMLInternalElementNode"),
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
          signature("nexml:rootEdge", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addAttributes(parent, "target" = object@target)
            if(length(object@length) > 0)
               addAttributes(parent, "length" = object@length)
            parent
          })
setAs("nexml:rootEdge", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName")))) 
setAs("nexml:rootEdge", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName")))) 
setAs("XMLInternalElementNode", "nexml:rootEdge",
      function(from) fromNeXML(new("nexml:rootEdge"), from))


################################ alternatively called "Taxon" by the schema


setClass("nexml:otu", contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:otu", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            obj
          })
setMethod("toNeXML", 
          signature("nexml:otu", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            parent
          })
setAs("nexml:otu", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:otu", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:otu",
      function(from) fromNeXML(new("nexml:otu"), from))

################################ alternatively called Taxa by the schema

setClass("ListOfotu", slots = c(names="character"),  
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "nexml:otu")))
                          "not all elements are otu objects"
                       else
                         TRUE)

###############################

setClass("nexml:otus", 
         slots = c(otu = "ListOfotu", names="character"), 
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:otus", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@otu <- new("ListOfotu", 
                              lapply(kids[names(kids) == "otu"], 
                                     as, "nexml:otu"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:otus", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@otu)
            parent
          })
setAs("nexml:otus", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:otus", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:otus",
      function(from) fromNeXML(new("nexml:otus"), from))

################################


setClass("ListOfedge", slots = c(names="character"), 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "nexml:edge")))
                          "not all elements are edge objects"
                       else
                         TRUE)



setClass("ListOfnode", slots = c(names="character"), 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "nexml:node")))
                          "not all elements are node objects"
                       else
                         TRUE)

################################## actually AbstractTree

setClass("nexml:tree", 
         slots = c(node = "ListOfnode", 
                        edge = "ListOfedge",
                        rootedge = "nexml:rootEdge"), # Actually AbstractRootEdge
         contains = "nexml:IDTagged")
setMethod("fromNeXML", 
          signature("nexml:tree", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@node <- new("ListOfnode", 
                            lapply(kids[names(kids) == "node"], 
                                   as, "nexml:node"))
            obj@edge <- new("ListOfedge", 
                            lapply(kids[names(kids) == "edge"], 
                                   as, "nexml:edge"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:tree", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@node)
            addChildren(parent, kids = object@edge)
            parent
          })
setAs("nexml:tree", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:tree", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:tree",
      function(from) fromNeXML(new("nexml:tree"), from))



################################################

setClass("ListOftree", slots = c(names="character"), contains = "list") # validity can contain tree or network nodes?

setClass("nexml:trees", 
         slots = c(tree = "ListOftree"), # Can contain networks...
         contains = "nexml:TaxaLinked")
setMethod("fromNeXML", 
          signature("nexml:trees", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@tree <- new("ListOftree", 
                            lapply(kids[names(kids) == "tree"], 
                                   as, "nexml:tree"))
            obj
          })
setMethod("toNeXML", 
          signature("nexml:trees", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = object@tree)
#            addChildren(parent, kids = object@network)
            parent
          })
setAs("nexml:trees", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("nexml:trees", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode(slot(from, "nexml:xmlName"))))
setAs("XMLInternalElementNode", "nexml:trees",
      function(from) fromNeXML(new("nexml:trees"), from))


####################################################

setClass("ListOfotus", slots = c(names="character"), contains = "list")
setClass("ListOftrees", slots = c(names="character"), contains = "list")
setClass("ListOfcharacters", slots = c(names="character"), contains = "list")

####################################################



nexml_namespaces <- 
  c("nex"   = "http://www.nexml.org/2009",
    "xsi"   = "http://www.w3.org/2001/XMLSchema-instance",
    "xml"   = "http://www.w3.org/XML/1998/namespace",
    "cdao"  = "http://purl.obolibrary.org/obo/cdao.owl",
    "xsd"   = "http://www.w3.org/2001/XMLSchema#",
    "dc"    = "http://purl.org/dc/elements/1.1/",
    "dcterms" = "http://purl.org/dc/terms/",
    "ter" = "http://purl.org/dc/terms/",
    "prism" = "http://prismstandard.org/namespaces/1.2/basic/",
    "cc"    = "http://creativecommons.org/ns#",
    "ncbi"  = "http://www.ncbi.nlm.nih.gov/taxonomy#",
    "tc"    = "http://rs.tdwg.org/ontology/voc/TaxonConcept#")


setClass("nexml", 
         slots = c(version = "character",
                        generator = "character",
                        "xsi:schemaLocation" = "character", # part of base?
                        namespaces = "character",           # part of base? 
                        otus = "ListOfotus",             
                        trees = "ListOftrees",
                        characters="ListOfcharacters"),
         prototype = prototype(version = "0.9",
                   generator = "RNeXML",
                   "xsi:schemaLocation" = "http://www.nexml.org/2009/nexml.xsd",
                   namespaces = c(nexml_namespaces, 
                                  "http://www.nexml.org/2009")),
         contains = "nexml:Annotated")

setMethod("fromNeXML", 
          signature("nexml", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()

            # handle attributes
            attrs <- xmlAttrs(from)
            obj@version <- attrs["version"]     # required attribute
            if(!is.na(attrs["generator"]))       # optional attribute
               obj@generator <- attrs["generator"]

            if(!is.na(attrs["xsi:schemaLocation"]))
              slot(obj, "xsi:schemaLocation") <- attrs["xsi:schemaLocation"]
            if(!is.na(attrs["schemaLocation"]))
              slot(obj, "xsi:schemaLocation") <- attrs["schemaLocation"]

            if(!is.na(attrs["xsi:type"]))
              slot(obj, "xsi:type") <- attrs["xsi:type"]
            if(!is.na(attrs["type"]))
              slot(obj, "xsi:type") <- attrs["type"]

            if(!is.na(attrs["about"]))
              obj@about <- attrs["about"]


            ns_defs <- xmlNamespaceDefinitions(from)
            ns <- sapply(ns_defs, `[[`, "uri")
            obj <- add_namespaces(ns, obj)

            # Handle children
            kids <- xmlChildren(from)
            # at least 1 OTU block is required 
            obj@otus <- new("ListOfotus", 
                            lapply(kids[names(kids) == "otus"], 
                                   as, "nexml:otus"))
            if("characters" %in% names(kids))
              obj@characters <- new("ListOfcharacters", 
                            lapply(kids[names(kids) == "characters"], 
                                   as, "nexml:characters"))
            if("trees" %in% names(kids))
              obj@trees <- new("ListOftrees", 
                            lapply(kids[names(kids) == "trees"], 
                                   as, "nexml:trees"))
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
            addChildren(parent, kids = object@otus) # a list of "otus" objects
            addChildren(parent, kids = object@trees) # a list of "trees" objects
            addChildren(parent, kids = object@characters) # a list of "characters" objects
            parent
          })

## NOTE: The root nexml element must have it's namespace
setAs("nexml", "XMLInternalNode",
      function(from) suppressWarnings(toNeXML(from, newXMLNode("nex:nexml", namespaceDefinitions = from@namespaces))))
setAs("nexml", "XMLInternalElementNode",
      function(from) suppressWarnings(toNeXML(from, newXMLNode("nex:nexml", namespaceDefinitions = from@namespaces))))
setAs("XMLInternalElementNode", "nexml",
      function(from) fromNeXML(new("nexml"), from))



#######################################################


