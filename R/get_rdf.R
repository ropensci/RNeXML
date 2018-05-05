#' Extract rdf-xml from a NeXML file
#' 
#' Extract rdf-xml from a NeXML file
#' @param file the name of a nexml file, or otherwise a nexml object. 
#' @return an RDF-XML object (XMLInternalDocument).  This can be manipulated with
#'   tools from the XML R package, or converted into a triplestore for use with 
#'   SPARQL queries from the rdflib R package.  
#' @export
#' @import httr XML
#' @importFrom xml2 read_xml
get_rdf <- function(file){

  
  ## Soft dependency on xslt
  if (!requireNamespace("xslt", quietly = TRUE)) {
    stop("xslt package required to convert to rdf",
         call. = FALSE)
  }
  xml_xslt <- getExportedValue("xslt", "xml_xslt")
  
    if(is(file, "nexml")){
      who <- tempfile()
      nexml_write(x=file, file=who)
      file <- who
    }
    to_rdf <- system.file("examples", "RDFa2RDFXML.xsl", package="RNeXML")
    rdf <- 
    rdf <- xml_xslt(xml2::read_xml(file), xml2::read_xml(to_rdf))

  rdf  
}




