#' validate nexml using the online validator tool
#' @param file path to the nexml file to validate
#' @details Requires an internet connection.  see http://www.nexml.org/nexml/phylows/validator for more information in debugging invalid files
#' @return TRUE if the file is valid, false otherwise
#' @export
#' @import httr XML
nexml_validate <- function(file){
#   xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", file)  
  # Consider providing a copy of the schema so this works offline?
  # xmlSchemaValidate potentially provides more useful debugging output...
  a = POST("http://www.nexml.org/nexml/phylows/validator", body=list(file = upload_file(file)))
  if(a$status_code == 201)
    TRUE
  else 
    FALSE
}
