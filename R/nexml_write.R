
nexml_write <- function(x, file = "nexml.xml"){
  out <- as(x, "XMLInternalDocument")
  saveXML(out, file = file)
}


setAs("XMLInternalNode", "XMLInternalDocument", function(from){
  newXMLDoc(node = from)
})


## phylo -> nexml -> InternalNode -> InternalDoc
setAs("phylo", "XMLInternalDocument", function(from){
  as(as(as(from, "nexml"), "XMLInternalNode"), "XMLInternalDocument")
})

setAs("multiPhylo", "nexml", function(from)
      new("nexml", trees = new("trees", tree = from)))

setAs("phylo", "nexml", function(from)
      new("nexml", trees = 
          new("trees", tree = 
              new("ListOfTree", list(as(from, "tree"))
              )
          )
      )
)



