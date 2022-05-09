


setGeneric("fromNeXML", function(obj, from) standardGeneric("fromNeXML")) 


setGeneric("toNeXML", 
           valueClass="XMLInternalElementNode",
           function(object, parent) 
             standardGeneric("toNeXML"))

# Rather verbose methods definitions manually reading and writing each class...
# Rather than just slot matching names, we explicitly map each attribute...

##############################

setClass("Base",
         slots = c('xsi:type' = "character"))
setMethod("toNeXML", 
          signature("Base", "XMLInternalElementNode"), 
          function(object, parent){
            type <- slot(object, "xsi:type")
            if(length(type) > 0){
              #if(is.na(pmatch("nex:", type)))  # nex or relevant namespace should come from default anyway
              #  type <- paste0("nex:", type)
              addAttributes(parent, 
                           "xsi:type" = type,  
                            suppressNamespaceWarning=TRUE) # We always define xsi namespace in the header... 
              }
            parent
          })
setMethod("fromNeXML", 
          signature("Base", "XMLInternalElementNode"),
          function(obj, from){
            if(!is.null(xmlAttrs(from))){
              if(!is.na(xmlAttrs(from)["type"]))  ## FIXME use [["type"]] or ["type"]
                slot(obj, "xsi:type") <- as.character(xmlAttrs(from)["type"])
              if(!is.na(xmlAttrs(from)["xsi:type"])) ## Shouldn't be necessary but seems to be for first test in test_inheritance.R...
                slot(obj, "xsi:type") <- as.character(xmlAttrs(from)["xsi:type"])
            }
            obj
          }
)

#########################

setClass("meta",
         slots = c(children = "list"), 
         contains = "Base")
setAs("XMLInternalNode", "meta", function(from){
  type <- xmlAttrs(from)["type"]
  if(is.na(type)) ## FIXME This is CRUDE
    type <- xmlAttrs(from)["xsi:type"]
  if(is.na(type)) # if still not defined...
    type = "meta"
  else
    type <- sub(".*:", "", type)[1] ## FIXME This is CRUDE
  fromNeXML(New(type), from)
})


#########################

setClass("LiteralMeta", 
         slots = c(id = "character",
                        property = "character", 
                        datatype = "character",
                        content = "character"),
         contains="meta")
setMethod("fromNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"),
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
            if(!is.na(attrs["id"]))
                 obj@id <- attrs[["id"]]
            obj
          }
)
setMethod("toNeXML", 
          signature("LiteralMeta", "XMLInternalElementNode"), 
          function(object, parent){
            parent <- callNextMethod()
            cnt <- object@content
            if (length(cnt) > 0 && is(cnt, "XMLString")) {
              # need to add back in the namespace definitions used by the
              # XML value to avoid a stream of warnings, even if this will
              # probably be redundant with the doc root
              parseXMLAndAdd(cnt, parent = parent, nsDefs = attr(cnt, "namespaces"))
              cnt <- character(0)
            }
            attrs <- c(id = unname(object@id),
                       property = unname(object@property),  # required
                       datatype = unname(object@datatype),  # optional
                       content = unname(cnt))               # required
            attrs <- plyr::compact(attrs)
            addAttributes(parent, .attrs = attrs)
})
setAs("XMLInternalElementNode", "LiteralMeta", function(from) fromNeXML(New("LiteralMeta"), from))
setAs("LiteralMeta", "XMLInternalElementNode", function(from) toNeXML(from, newXMLNode("meta")))
setAs("LiteralMeta", "XMLInternalNode", function(from) toNeXML(from, newXMLNode("meta")))


##############################################

setClass("ResourceMeta", 
         slots = c(id = "character",
                        rel = "character", 
                        href = "character"),
         contains="meta")
setMethod("fromNeXML", 
          signature("ResourceMeta", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            if(!is.na(attrs["href"]))
              obj@href <- attrs[["href"]]
            if(!is.na(attrs["id"]))
              obj@id <- attrs[["id"]]
            if(!is.na(attrs[["rel"]]))
                 obj@rel <- attrs[["rel"]]
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@children <- lapply(kids[names(kids) == "meta"], as, "meta")
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
            if (length(object@children) > 0)
              addChildren(parent, kids = lcapply(object@children, as, "XMLInternalNode"))
            parent
})
setAs("XMLInternalElementNode", "ResourceMeta", function(from) fromNeXML(New("ResourceMeta"), from))
setAs("ResourceMeta", "XMLInternalNode", function(from) toNeXML(from, newXMLNode("meta")))
setAs("ResourceMeta", "XMLInternalElementNode", function(from) toNeXML(from, newXMLNode("meta")))



##############################################


setClass("ListOfmeta",
         slots = c(names="character"),
         contains = "list",
         validity = function(object)
           if(!all(sapply(object, is, "meta")))
             "not all elements are meta objects"
           else
             TRUE
         )


###############################################

#' Class of objects that have metadata as lists of meta elements
#'
#' @slot meta list of `meta` objects
#' @slot about for RDF extraction, the identifier for the resource that this
#'   object is about
setClass("Annotated",
         slots = c(meta = "ListOfmeta",
                        about = "character"),
         contains = "Base")
setMethod("fromNeXML",
          signature("Annotated", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@meta <- New("ListOfmeta",
                              lapply(kids[names(kids) == "meta"], 
                                     as, "meta"))
            if(!is.null(xmlAttrs(from)))
              if(!is.na(xmlAttrs(from)["about"]))
                 obj@about <- xmlAttrs(from)["about"]
            obj
})
setMethod("toNeXML", 
          signature("Annotated", "XMLInternalElementNode"), 
           function(object, parent){
             parent <- callNextMethod()
             addChildren(parent, kids = lcapply(object@meta, as, "XMLInternalNode"))
             if(length(object@about) > 0)
               addAttributes(parent, "about" = object@about)
             parent
          })


######################################################

setClass("Labelled",
         slots = c(label = "character"),
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
         slots = c(id = "character"),
         contains = "Labelled")
setMethod("fromNeXML", 
          signature("IDTagged", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            if(!is.na(xmlAttrs(from)["id"]))
                 obj@id <- as.character(xmlAttrs(from)["id"])
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
         slots = c(otu = "character"),
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("OptionalTaxonLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otu"]))
               obj@otu <- as.character(xmlAttrs(from)["otu"])
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
         slots = c(otus = "character"),
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("TaxaLinked", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
             if(!is.na(xmlAttrs(from)["otus"]))
               obj@otus <- as.character(xmlAttrs(from)["otus"])
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
         slots = c(root = "logical"),
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
      function(from) fromNeXML(nexml.node(), from))



################################ Really AbstractEdge

setClass("edge", 
         slots = c(source = "character",
                        target = "character", 
                        length = "numeric"), 
         contains="IDTagged")
setMethod("fromNeXML", 
          signature("edge", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            attrs <- xmlAttrs(from)
            if (!is.na(attrs["source"]))
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
            if(is(object, "rootEdge"))
              xmlName(parent) <- "rootedge"
            else
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
      function(from) fromNeXML(nexml.edge(), from))

# virtual class, allows asking is(edge, "rootEdge") to determine whether
# a given edge object is a root edge or not
setClass("rootEdge") # creates a virtual class
setIs("edge", "rootEdge",
      test = function(Object) {
        length(Object@target) > 0 && length(Object@source) == 0
      },
      replace = function(Object, value) {
        stop("rootEdge is virtual, cannot replace")
      })

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
      function(from) fromNeXML(nexml.otu(), from))

################################ alternatively called Taxa by the schema

setClass("ListOfotu", slots = c(names="character"),  
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "otu")))
                          "not all elements are otu objects"
                       else
                         TRUE)

###############################

setClass("otus", 
         slots = c(otu = "ListOfotu", names="character"), 
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("otus", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            if(length(kids) > 0)
              obj@otu <- New("ListOfotu",
                              lapply(kids[names(kids) == "otu"], 
                                     as, "otu"))
            obj
          })
setMethod("toNeXML", 
          signature("otus", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = lcapply(object@otu, as, "XMLInternalNode"))
            parent
          })
setAs("otus", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("otus")))
setAs("otus", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("otus")))
setAs("XMLInternalElementNode", "otus",
      function(from) fromNeXML(nexml.otus(), from))

################################


setClass("ListOfedge", slots = c(names="character"), 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "edge")))
                          "not all elements are edge objects"
                       else
                         TRUE)



setClass("ListOfnode", slots = c(names="character"), 
         contains = "list",
         validity = function(object)
                       if(!all(sapply(object, is, "node")))
                          "not all elements are node objects"
                       else
                         TRUE)

################################## actually AbstractTree

## Thanks Gabor for work-around from S4 namespaces collision issue
# https://github.com/ropensci/RNeXML/issues/251#issuecomment-1031996431
tryCatch({
  rlang::env_unlock(asNamespace("cli"))
  removeClass("tree", asNamespace("cli"))
  rlang::env_lock(asNamespace("cli"))
}, error = function(err) NULL)

setClass("tree", 
         slots = c(node = "ListOfnode", 
                        edge = "ListOfedge",
                        rootedge = "edge"),
         contains = "IDTagged")
setMethod("fromNeXML", 
          signature("tree", "XMLInternalElementNode"),
          function(obj, from){
            .cacheNextMethod()
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@node <- New("ListOfnode",
                            lapply(kids[names(kids) == "node"], 
                                   as, "node"))
            obj@edge <- New("ListOfedge",
                            lapply(kids[names(kids) == "edge"], 
                                   as, "edge"))
            rootEdge <- kids[names(kids) == "rootedge"]
            if (length(rootEdge) > 0)
              obj@rootedge <- as(rootEdge[[1]], "edge")
            obj
          })
setMethod("toNeXML", 
          signature("tree", "XMLInternalElementNode"),
          function(object, parent){
            .cacheNextMethod()
            parent <- callNextMethod()
            addChildren(parent, kids = lcapply(object@node, as, "XMLInternalNode"))
            addChildren(parent, kids = lcapply(object@edge, as, "XMLInternalNode"))
            if (is(object@rootedge, "rootEdge"))
              addChildren(parent, as(object@rootedge, "XMLInternalNode"))
            parent
          })
setAs("tree", "XMLInternalNode",
      function(from) .callGeneric("toNeXML", from, newXMLNode("tree")))
setAs("tree", "XMLInternalElementNode",
      function(from) .callGeneric("toNeXML", from, newXMLNode("tree")))
setAs("XMLInternalElementNode", "tree",
      function(from) .callGeneric("fromNeXML", nexml.tree(), from))



################################################

setClass("ListOftree", slots = c(names="character"), contains = "list") # validity can contain tree or network nodes?

setClass("trees", 
         slots = c(tree = "ListOftree"), # Can contain networks...
         contains = "TaxaLinked")
setMethod("fromNeXML", 
          signature("trees", "XMLInternalElementNode"),
          function(obj, from){
            obj <- callNextMethod()
            kids <- xmlChildren(from)
            obj@tree <- New("ListOftree",
                            lapply(kids[names(kids) == "tree"], 
                                   as, "tree"))
            obj
          })
setMethod("toNeXML", 
          signature("trees", "XMLInternalElementNode"),
          function(object, parent){
            parent <- callNextMethod()
            addChildren(parent, kids = lapply(object@tree, as, "XMLInternalNode"))
#            addChildren(parent, kids = object@network)
            parent
          })
setAs("trees", "XMLInternalNode",
      function(from) toNeXML(from, newXMLNode("trees")))
setAs("trees", "XMLInternalElementNode",
      function(from) toNeXML(from, newXMLNode("trees")))
setAs("XMLInternalElementNode", "trees",
      function(from) fromNeXML(nexml.trees(), from))


####################################################

setClass("ListOfotus", slots = c(names="character"), contains = "list")
setClass("ListOftrees", slots = c(names="character"), contains = "list")
setClass("ListOfcharacters", slots = c(names="character"), contains = "list")

####################################################



nexml_namespaces <- 
  c("nex"   = "http://www.nexml.org/2009",
    "xsi"   = "http://www.w3.org/2001/XMLSchema-instance",
    "xml"   = "http://www.w3.org/XML/1998/namespace",
    "cdao"  = "http://purl.obolibrary.org/obo/",
    "xsd"   = "http://www.w3.org/2001/XMLSchema#",
    "dc"    = "http://purl.org/dc/elements/1.1/",
    "dcterms" = "http://purl.org/dc/terms/",
    "prism" = "http://prismstandard.org/namespaces/1.2/basic/",
    "cc"    = "http://creativecommons.org/ns#",
    "ncbi"  = "http://www.ncbi.nlm.nih.gov/taxonomy#",
    "tc"    = "http://rs.tdwg.org/ontology/voc/TaxonConcept#")

#' Class representing a NeXML document
#'
#' The `nexml` class represents a NeXML document, and is the top of the
#' class hierarchy defined in this package, corresponding to the root node
#' of the corresponding XML document.
#'
#' Normally objects of this type are created by the package as a result of
#' reading a NeXML file, or of converting from another type, such as
#' `ape::phylo`. Also, interacting directly with the slots of the class is
#' normally not necessary. Instead, use the `get_XXX()` and `add_XXX()`
#' functions in the API. 
#' @slot trees list, corresponding to the list of `<trees/>` elements in
#'   NeXML. Elements will be of class `trees`.
#' @slot characters list, corresponding to the list of `<characters/>`
#'   elements in NeXML. Elements will be of class `characters`.
#' @slot otus list, corresponding to the list of `<otus/>` elements in NeXML.
#'   Elements will be of class `otus`.
#' @slot about inherited, see [Annotated][Annotated-class]
#' @slot meta inherited, see [Annotated][Annotated-class]
#' @slot xsi:type for internal use
#' @slot version NeXML schema version, do not change
#' @slot generator name of software generating the XML
#' @slot xsi:schemaLocation for internal use, do not change
#' @slot namespaces named character vector giving the XML namespaces
#' @seealso [read.nexml()]
#' @examples
#' nex <- nexml() # a nexml object with no further content
#' nex <- new("nexml") # accomplishes the same thing
#' nex@generator
#' length(nex@trees)
#'
#' data(bird.orders)
#' nex <- as(bird.orders, "nexml")
#' summary(nex)
#' length(nex@trees)
#' @export nexml
#' @exportClass nexml
nexml <- setClass("nexml",
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

            # add our own namespaces, which will skip already present prefixes;
            # also add a base namespace if there isn't one already
            ourNS <- nexml_namespaces;
            if (! any(names(obj@namespaces) == ""))
              ourNS <- c(ourNS, unname(nexml_namespaces["nex"]))
            obj <- add_namespaces(ourNS, nexml = obj)

            # Handle children
            kids <- xmlChildren(from)
            # at least 1 OTU block is required 
            obj@otus <- New("ListOfotus",
                            lapply(kids[names(kids) == "otus"], 
                                   as, "otus"))
            if("characters" %in% names(kids))
              obj@characters <- New("ListOfcharacters",
                            lapply(kids[names(kids) == "characters"], 
                                   as, "characters"))
            if("trees" %in% names(kids))
              obj@trees <- New("ListOftrees",
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
      function(from) fromNeXML(nexml(namespaces = character(0)), from))



#######################################################


