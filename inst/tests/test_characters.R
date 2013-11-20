

require(XML)
require(RNeXML)

f <- system.file("examples", "comp_analysis.xml", package="RNeXML")
doc <- xmlParse(f)
root <- xmlRoot(doc)

char <- as(root[["characters"]][["format"]][["char"]], "char")
as(char, "XMLInternalElementNode")


format <- as(root[["characters"]][["format"]], "format")
as(format, "XMLInternalElementNode")



as(root[["characters"]], "characters")

