library(xslt)
library(xml2)
library(jsonld)
library(rdflib)
library(magrittr)

## Apply stylesheet to map NeXML RDFa -> RDF
## Does not map the full NeXML data
download.file("https://raw.githubusercontent.com/ropensci/RNeXML/master/inst/examples/RDFa2RDFXML.xsl", "toRDF.xsl")
download.file("https://raw.githubusercontent.com/ropensci/RNeXML/master/inst/examples/multitrees.xml", "example.xml")
rdf <- xslt::xml_xslt(read_xml("example.xml"), read_xml("toRDF.xsl"))

## Save RDF to file
write_xml(rdf, "rdf.xml")

rdf_parse("rdf.xml")

## JSON-LD RDF parser only speaks nquads, so use rdflib to convert:
rdflib::rdf_serialize(rdf_parse("rdf.xml"), "example.nquads", "nquads")
jsonld::jsonld_from_rdf("example.nquads")
