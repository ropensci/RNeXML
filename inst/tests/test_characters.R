

require(XML)
require(RNeXML)


## Add unit tests to make sure anything that doesn't have to have attributes doesn't break parsing.  

f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
doc <- xmlParse(f)
root <- xmlRoot(doc)

char <- as(root[["characters"]][["format"]][["char"]], "char")
as(char, "XMLInternalElementNode")


format <- as(root[["characters"]][["format"]], "format")
as(format, "XMLInternalElementNode")

matrix <- as(root[["characters"]][["matrix"]], "obsmatrix")
as(matrix, "XMLInternalElementNode")



characters <- as(root[["characters"]], "characters")
as(characters, "XMLInternalElementNode")


## 
test_that("add_otu works", {
  nex <- read.nexml(f)
  orig <- get_taxa(nex)
  x <- get_characters_list(nex)
  nex@otus[[1]]@otu <- new("ListOfotu", nex@otus[[1]]@otu[1:5]) # chop off some of the otu values 
  nex2 <- add_otu(nex, x, 1) # add them back 
## should have same contents as orig... 
  get_taxa(nex2)
  expect_identical(sort(orig), sort(get_taxa(nex2)))

## Note that otu ids are not unique when we chop them off ...
})

get_characters(nexml)

