#' Extract rdf-xml from a NeXML file
#' 
#' Extract rdf-xml from a NeXML file
#' @param file the name of a nexml file, or otherwise a nexml object. 
#' @return an RDF-XML object (XMLInternalDocument).  This can be manipulated with
#'   tools from the XML R package, or converted into a triplestore for use with 
#'   SPARQL queries from the rrdf R package.  
#' @export
#' @import httr XML
#' @examples \dontrun{
#' f <- system.file("examples", "meta_example.xml", package="RNeXML")
#' rdf <- get_rdf(f)
#'
#' ## Query the rdf with XPath: 
#' xpathSApply(rdf, "//dc:title", xmlValue) 
#' 
#' ## Write to a file and read in with rrdf
#' saveXML(rdf, "rdf_meta.xml")
#' library(rrdf)
#' lib <- load.rdf("rdf_meta.xml")
#' 
#' ## Perform a SPARQL query:
#' sparql.rdf(lib, "SELECT ?title WHERE { ?x <http://purl.org/dc/elements/1.1/title> ?title}")
#' }
get_rdf <- function(file){
  clean <- FALSE
  if(is(file, "nexml")){
    write.nexml(file, file = "tmpnexmlrdf.xml")
    clean <- TRUE
    file <- "tmpnexmlrdf.xml"
  }
  response <- POST("http://rdf-translator.appspot.com/convert/rdfa/xml/content", 
                   body=list(content=upload_file(file)))
  doc <- content(response, "parsed", "text/xml")
  if(clean)
    unlink("tmpnexmlrdf.xml")
  doc
}


