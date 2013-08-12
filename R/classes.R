nexml_namespaces <- 
  c("xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xml" = "http://www.w3.org/XML/1998/namespace",
    "nex" = "http://www.nexml.org/2009",
    "cdao" = "http://www.evolutionaryontology.org/cdao/1.0/cdao.owl#",
    "xsd" = "http://www.w3.org/2001/XMLSchema#")

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

setClass("ListOfMeta", contains = "list",
          validity = function(object)
                       if(!all(sapply(object, is, "meta")))
                          "not all elements are meta objects"
                       else
                         TRUE)

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

setClass("otu", 
         representation(id = "character"))
setClass("ListOfotu", contains = "list") 

setClass("otus", 
         representation(otu = "ListOfotu"))


setClass("tree",
         representation(id          = "character", 
                        'xsi:type'  = "character", 
                        label       = "character", 
                        nodes       = "ListOfnode", 
                        edges       = "ListOfedge"))

setClass("ListOfTree", contains = "list") # Also includes "networks"
setClass("trees", 
         representation(tree = "ListOfTree"))



setClass("nexml",
         representation(version = "character",
                        generator = "character",
                        "xsi:schemaLocation" = "character",
                        namespaces = "character",
                        meta = "ListOfMeta",
                        otus = "otus",
                        trees = "trees"),
         prototype(version = "0.9",
                   generator = "RNeXML",
                   "xsi:schemaLocation" = "http://www.nexml.org/2009 ../xsd/nexml.xsd",
                   namespaces = nexml_namespaces))



