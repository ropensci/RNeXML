context("parsing and serializing from S4")


test_that("We can parse a NeXML file to an S4 RNeXML::tree object and convert back into NeXML", {
  library(XML)
  library(RNeXML)

  f <- system.file("examples", "trees.xml", package="RNeXML")
  doc <- xmlParse(f)
  root <- xmlRoot(doc)

## Coerce the first XML tree node into S4
  tree <- as(root[["trees"]][["tree"]], "tree")

  suppressWarnings({  ## Warns on xsi namespace definition missing (FIXME), and on In removeNodes.list(kids) (no idea) 
    as(tree, "XMLInternalElementNode")
  })
})
