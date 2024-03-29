#ONLINE_VALIDATOR <- "http://162.13.187.155/nexml/phylows/validator"
#CANONICAL_SCHEMA <- "http://162.13.187.155/nexml/xsd/nexml.xsd"
ONLINE_VALIDATOR <- "http://www.nexml.org/nexml/phylows/validator"
CANONICAL_SCHEMA <- "http://www.nexml.org/2009/nexml.xsd"
CANONICAL_SCHEMA <-  "https://github.com/nexml/nexml/raw/master/xsd/nexml.xsd"
#' validate nexml using the online validator tool
#' @param file path to the nexml file to validate
#' @param schema URL of schema (for fallback method only, set by default).
#' @param local logical, if TRUE we skip the online validator and rely only on pure XML-schema validation.  This may fail to detect invalid use of some semantic elements.
#' @details Requires an internet connection if local=FALSE.  see http://www.nexml.org/nexml/phylows/validator for more information in debugging invalid files
#' @return TRUE if the file is valid, FALSE or error message otherwise
#' @export
#' @import httr XML
#' @examples \dontrun{
#' data(bird.orders)
#' birds <- nexml_write(bird.orders, "birds_orders.xml")
#' nexml_validate("birds_orders.xml")
#' unlink("birds_orders.xml") # delete file to clean up
#' }
nexml_validate <- function(file, schema=system.file("xsd/nexml.xsd", package="RNeXML"), local = TRUE){
  
  if(local) {
    return( nexml_schema_validate(file, schema=schema) )
  }
  
  a = POST(ONLINE_VALIDATOR, body=list(file = upload_file(file)))
  if(a$status_code %in% c(200,201)){
    TRUE
  } else if(a$status_code == 504){
    warning("Online validator timed out, trying schema-only validation.")
    nexml_schema_validate(file, schema=schema)

  } else if(a$status_code == 400){
    warning(paste("Validation failed, error messages:",
         xpathSApply(htmlParse(content(a, "text")), 
                     "//li[contains(@class, 'error') or contains(@class, 'fatal')]", xmlValue)
         ))
    FALSE
  } else {
    warning(paste("Unable to reach validator. status code:", a$status_code, ".  Message:\n\n", content(a, "text")))
    NULL
  }
}




nexml_schema_validate <- function(file, schema=CANONICAL_SCHEMA){

      xml2::xml_validate(xml2::read_xml(file), xml2::read_xml(schema)) 
   
    
}
#xmlSchemaValidate(xmlSchemaParse(content(a, "text"), asText=TRUE), file)   # fails to get other remote resources



