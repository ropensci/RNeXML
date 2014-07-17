#' Extract rdf-xml from a NeXML file
#' 
#' Extract rdf-xml from a NeXML file
#' @param file the name of a nexml file, or otherwise a nexml object. 
#' @return an RDF-XML object (XMLInternalDocument).  This can be manipulated with
#'   tools from the XML R package, or converted into a triplestore for use with 
#'   SPARQL queries from the rrdf R package.  
#' @export
#' @import httr XML
# @import Sxslt # not yet
#' @examples \dontrun{
#' f <- system.file("examples", "meta_example.xml", package="RNeXML")
#' rdf <- get_rdf(f)
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

  success <- require("Sxslt", , 
                     character.only = TRUE, 
                     quietly = TRUE) # Wait until package is available on CRAN to formally depend on it.  

  if(success){
    if(is(file, "nexml")){
      who <- tempfile()
      nexml_write(x=file, file=who)
      file <- who
    }
    to_rdf <- system.file("examples", "RDFa2RDFXML.xsl", package="RNeXML")
    rdf <- xsltApplyStyleSheet(file, to_rdf)
  } else {
    warning("Package Sxslt not available, please install it from www.omegahat.org") 
  }
  rdf  
}




