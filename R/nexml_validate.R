#' validate nexml using the online validator tool
#' @param file path to the nexml file to validate
#' @details Requires an internet connection.  see http://www.nexml.org/nexml/phylows/validator for more information in debugging invalid files
#' @return TRUE if the file is valid, FALSE or error message otherwise
#' @export
#' @import httr XML
#' @examples \dontrun{
#' data(bird.orders)
#' birds <- nexml_write(bird.orders, "birds_orders.xml")
#' nexml_validate("bird_orders.xml")
#' unlink("bird_orders.xml") # delete file to clean up
#' }
nexml_validate <- function(file){
#   xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", file)  
  # Consider providing a copy of the schema so this works offline?
  # xmlSchemaValidate potentially provides more useful debugging output...
  a = POST("http://www.nexml.org/nexml/phylows/validator", body=list(file = upload_file(file)))
  if(a$status_code == 201){
    TRUE
  } else if(a$staus_code == 504){
    warning("Online validator timed out, trying schema-only validation.")
    nexml_schema_validate(file)

  } else if(a$status_code == 400){
    warning(paste("Validation failed, error messages:",
         xpathSApply(htmlParse(content(a, "text")), 
                     "//li[contains(@class, 'error') or contains(@class, 'fatal')]", xmlValue)
         ))
    FALSE
  } else {
    warning(paste("Unable to reach validator, status code", a$status_code, "message", content(a, "text")))
    NULL
  }
}

nexml_schema_validate <- function(file){
  a = GET("http://www.nexml.org/2009/nexml.xsd")
  if(a$status_code == 200){
    result <- xmlSchemaValidate("http://www.nexml.org/2009/nexml.xsd", file) 
    if(length(result$errors) == 0){
      TRUE
    } else {
      warning(paste(result$errors))
      FALSE
    }
  } else {
    warning("Unable to obtain schema, couldn't validate")
    NULL
  }
    
}
#xmlSchemaValidate(xmlSchemaParse(content(a, "text"), asText=TRUE), file)   # fails to get other remote resources


## Helper function for testing
#' @importFrom testthat expect_true
expect_true_or_null <- function(result){
  validated <- if(is.null(result)){
    TRUE
  } else if(is.logical(result)){
    if(result){
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
  expect_true(validated)
}

