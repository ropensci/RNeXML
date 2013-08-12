context("serializing")

test_that("We can serialize S4 RNeXML into XML",{
  library(XML)
  library(RNeXML)

  root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
  tree <- as(root[["trees"]][["tree"]], "tree")

  ## Test
  nexml_write(tree, "test.xml")

  ##  Clean up
  unlink("test.xml")

  })


test_that("We generate valid NeXML from our S4 object", {
})

