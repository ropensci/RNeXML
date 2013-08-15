#' Write nexml files
#' 
#' @aliases write.nexml
#' @param x any phylogeny object (e.g. phylo, phylo4, or internal type)
#' @param file the name of the file to write out
#' @return Writes out a nexml file
#' @import ape 
#' @import XML
#' @export
nexml_write <- function(x, file = "nexml.xml"){
  out <- as(as(x, "nexml"), "XMLInternalDocument")
  saveXML(out, file = file)
}




################################################ Going back from S4 to XML, "Write Methods" ####################

setAs("tree", "XMLInternalElementNode",
      function(from){
        tree <- from
        obj <- newXMLNode("tree", 
                          attrs = c(id = tree@id, 
                                    label = tree@label, 
                                    'xsi:type' = slot(tree, 'xsi:type'))) 
        addChildren(obj, kids = tree@nodes) # kids envokes coercion 
        addChildren(obj, kids = tree@edges) # kids envokes coercion 
      })

setAs("tree", "XMLInternalNode", function(from) as(from, "XMLInternalElementNode"))

setAs("edge", "XMLInternalNode", 
      function(from){
         x <- from
         
         ## Avoid returning empty nodes -- is this necessary?
         if(is_class_empty(x@meta))
          kids <- list()
         else
          kids <- x@meta


         newXMLNode("edge", attrs = c(source = unname(x@source), 
                                      target = unname(x@target), 
                                      id = unname(x@id), 
                                      length = x@length), 
                    kids = kids)
      })

setAs("node", "XMLInternalNode", 
      function(from){
         x <- from
         
         ## Avoid returning empty nodes -- is this necessary?
         if(is_class_empty(x@meta))
          kids <- list()
         else
          kids <- x@meta


         newXMLNode("node", attrs = c(id = unname(x@id), 
                                      label = unname(x@label), 
                                      otu = unname(x@otu)), 
                    kids = kids)
      })


setAs("meta", "XMLInternalNode",
      function(from){
        x <- from
        attrs = c(id = x@id, 
                  property = x@property, 
                  content = x@content, 
                  'xsi:type' = slot(x, 'xsi:type'),  
                  datatype = x@datatype)
        newXMLNode("meta", attrs = attrs)
      })


setAs("otu", "XMLInternalNode",
      function(from){
        x <- from
        attrs = c(id = x@id)
        newXMLNode("otu", attrs = attrs)
      })
setAs("otu", "XMLInternalElementNode", # probably not needed 
      function(from) as(from, "XMLInternalNode"))


setAs("otus", "XMLInternalNode", function(from)
  newXMLNode("otus", 
             lapply(from@otu, as, "XMLInternalNode"),
             attrs = c(id = unname(from@id),
                       label = unname(from@label))))



setAs("trees", "XMLInternalNode", function(from)
  newXMLNode("trees", 
             lapply(from@tree, as, "XMLInternalNode"),
             attrs = c(id = unname(from@id),
                        label = unname(from@label),
                        otus = unname(from@otus))
                        ))




setAs("nexml", "XMLInternalNode", function(from){
  nexml <- newXMLNode("nex:nexml", 
            attrs = c(version = from@version, generator = from@generator,
                      "xsi:schemaLocation" = slot(from, "xsi:schemaLocation")), 
            namespaceDefinitions = from@namespaces)
  if(!is_class_empty(from@meta))
    addChildren(nexml, from@meta)
  if(!is_class_empty(from@otus))
    addChildren(nexml, from@otus)
  addChildren(nexml,  from@trees)
              
  nexml
})





is_class_empty <- function(x){
  nms <- slotNames(x)
  !as.logical(sum(sapply(sapply(nms, slot, object = x), length)))
}



############## Promotion methods ########

## want generator methods that can handle id creation better

setAs("tree", "nexml", function(from){
  trees = as(from, "trees")
  otus = as(from, "otus")
  otus@id = "tax1" #UUIDgenerate()
  trees@id = "Trees" #UUIDgenerate()
  trees@otus = otus@id
  new("nexml", 
      trees = trees,
      otus = otus)
})

setAs("tree", "otus", function(from){
  nodes_with_otus <- 
    plyr::compact(sapply(from@nodes, 
                         function(n) if(length(n@otu > 0)) n))
  new("otus", otu=new("ListOfotu", lapply(nodes_with_otus, as, "otu")))
})

setAs("ListOfnode", "otus", function(from)
  new("otus", otu = from))

setAs("node", "otu", function(from)
  new("otu", id = unname(from@otu)))


setAs("tree", "trees", function(from)
  new("trees", tree = new("ListOfTree", list(from))))

setAs("XMLInternalNode", "XMLInternalDocument", 
      function(from) newXMLDoc(node = from))

setAs("nexml", "XMLInternalDocument", function(from)
  as(as(from, "XMLInternalNode"), "XMLInternalDocument"))


