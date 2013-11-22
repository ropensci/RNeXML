

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




nex <- read.nexml(f)
characters(nex)


