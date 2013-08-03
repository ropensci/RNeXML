context("ape")


test_that("We can successfully plot an ape object using base functions", {

  library(XML)
  library(RNeXML)

  f <- system.file("examples", "trees.xml", package="RNeXML")
  doc <- xmlParse(f)
  root <- xmlRoot(doc)

## Coerce the first XML tree node into S4
  tree <- as(root[["trees"]][["tree"]], "tree")

## Coerce the S4 into phylo S3 object
  phy <- as(tree, "phylo")

## Demonstrate that we now have a phylo object
  library(ape)
  plot(phy)


})


test_that("We can convert from ape back into a S4 RNeXML::tree object", {
  ## We could just use an existing phylo tree.  
  ## Start by parsing NeXML into an RNeXML::tree 
  library(XML)
  root <- xmlRoot(xmlParse(system.file("examples", "trees.xml", package="RNeXML")))
  tree <- as(root[["trees"]][["tree"]], "tree")

  ## Convert to an ape::phylo 
  phy <- as(tree, "phylo")

  ## Attempt to convert back to RNeXML::tree S4
  re_tree <- as(phy, "tree")

  ## And convert back again to an ape::phylo
  re_phy <- as(re_tree, "phylo")

  ## Does it plot?
  library(ape)
  plot(re_phy)

  ## Does it match our original phylo tree?
  expect_identical(phy, re_phy)

  # Note that "identical" will not necessarily hold for 
  # `tree` and `re_tree`, since conversion to phylo is lossy 

})




