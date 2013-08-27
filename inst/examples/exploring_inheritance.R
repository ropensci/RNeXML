

### Hacky way to do this....


fromNeXML <- function(from, base, extension){
        obj <- as(as(from, base), extension)        
        obj <- XML:::addXMLAttributes(obj, xmlAttrs(from))
        obj 
      }


setAs("XMLInternalElementNode", "IDTagged",
      function(from) fromNeXML(from, "Labelled", "IDTagged"))


setAs("XMLInternalElementNode", "OptionalTaxonLinked",
      function(from) fromNeXML(from, "IDTagged", "OptionalTaxonLinked"))

setClass("node", 
         representation(root = "logical"),
         contains = "OptionalTaxonLinked")
setAs("XMLInternalElementNode", "node",
      function(from) fromNeXML(from, "OptionalTaxonLinked", "node"))


