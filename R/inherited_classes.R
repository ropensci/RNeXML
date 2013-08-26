setClass("base",
         representation( ))

setClass("annotated",
         representation(meta = "ListOfmeta",
                        about = "character"   # optional attribute
                        ),
         contains = "base")

setClass("labelled",
         representation(label = "character"),
         contains = "annotated")

setClass("idTagged",
         representation(id = "character"),
         contains = "labelled")


setClass("optionalTaxonLinked", 
         representation(otu = "character"),
         contains = "idTagged")

setClass("abstractNode", 
         representation(root = "logical"),
         contains = "optionalTaxonLinked")
