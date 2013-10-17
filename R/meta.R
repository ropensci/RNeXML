## Utilities for adding additional metadata


#' Constructor function for metadata nodes
#' 
#' @param property specify the ontological definition together with it's namespace, e.g. dc:title
#' @param datatype optional RDFa field
#' @param content content of the metadata field
#' @param rel Ontological definition of the reference provided in href 
#' @param href A link to some reference
#' @param id optional id element
#' @param type optional xsi:type.  If not given, will use either "LiteralMeta" or "ResourceMeta" as 
#'  determined by the presence of either a property or a href value.  
#' @details User must either provide property+content or rel+href.  Mixing these will result in potential garbage. 
#' The datatype attribute will be detected automatically from the class of the content argument.  Maps from R class
#' to schema datatypes are as follows: 
#' character - xs:string, 
#' Date - xs:date,
#' integer - xs:integer,
#' numeric - xs:decimal
#' logical - xs:boolean
#' 
#' @export 
#' @seealso \code{\link{nexml_write}}
# FIXME generate id elements??
meta <- function(property = character(0), 
                 datatype = character(0), 
                 content = character(0), 
                 rel = character(0), 
                 href = character(0), 
                 id = character(0),
                 type = character(0),
                 children = NULL){
  if(is.logical(content))
    datatype <- "xsd:boolean"
  else if(is(content, "Date"))
    datatype <- "xsd:date"
  else if(is.numeric(content))
    datatype <- "xsd:decimal"
  else if(is.character(content))
    datatype <- "xsd:string"
  else if(is.integer(content))
    datatype <- "xsd:integer"
  else 
    datatype <- "xsd:string"

  if(length(property) > 0)
    new("meta", content = content, datatype = datatype, 
        property = property, id = id, 'xsi:type' = "LiteralMeta")
  else if(length(rel) > 0)
    new("meta", rel = rel, href = href, 
        id = id, 'xsi:type' = "ResourceMeta")
  else 
    new("meta", content = content, datatype = datatype, 
        rel = rel, href = href, id = id, 'xsi:type' = type)
}


nexml_citation <- function(obj){
  if(is(obj, "bibentry")){
    out <- lapply(obj, function(obj){
      if(length(grep("--", obj$pages)) > 0){
        start_page <- strsplit(obj$pages, "--")[[1]][[1]]
        end_page <- strsplit(obj$pages, "--")[[1]][[2]]
      } else if(length(grep("-", obj$pages)) > 0){
        start_page <- strsplit(obj$pages, "-")[[1]][[1]]
        end_page <- strsplit(obj$pages, "-")[[1]][[2]]
      }
      list_of_metadata_nodes <- c(list(
        meta(content=obj$volume, 
            datatype="xsd:string", 
            property="prism:volume"),
        meta(content=obj$journal, 
            datatype="xsd:string",
            property="dc:publisher"),
        meta(content=obj$journal, 
            datatype="xsd:string",
            property="prism:publicationName"),
        meta(content = end_page, 
            datatype="xsd:string", 
            property="prism:endingPage"),
        meta(content=start_page, 
            datatype="xsd:string",
            property="prism:startingPage"),
        meta(content=obj$year, 
            datatype="xsd:string",
            property="prism:publicationDate"),
        meta(content=obj$title,
            datatype="xsd:string", 
            property="dc:title"),
        meta(content=format(obj, "text"), 
            datatype="xsd:string",
            property="dcterms:bibliographicCitation")
        ),
        lapply(obj$author, function(x){
        meta(content = format(x, c("given", "family")),
             property="dc:contributor", 
             datatype = "xsd:string") 
        })
      )
    })
  }
}




creator <- function(creator){
  creator <- as.person(creator)
  string <- format(creator, include=c("family", "given"), 
                   braces = list(family=c("", ",")))
  new("meta", 
      content=string, 
      datatype="xsd:string", 
      property="dc:creator",
      'xsi:type'="LiteralMeta")

}

title <- function(title){
  new("meta", 
      content=title,
      datatype="xsd:string",
      property="dc:title",
      'xsi:type'="LiteralMeta")
}



description <- function(description){
  new("meta", 
      content=description,
      datatype="xsd:string",
      property="dc:description",
      'xsi:type'="LiteralMeta")
}

pubdate <- function(pubdate=Sys.Date()){
  new("meta",
      content=format(pubdate),
      datatype="xsd:string",
      property="dc:date",
      'xsi:type'="LiteralMeta")
}

publisher <- function(publisher){
  new("meta", 
      content=publisher,
      datatype="xsd:string",
      property="dc:publisher",
      'xsi:type'="LiteralMeta")
}

rights <- function(rights="CC0"){
  if(rights == "CC0")
    new("ResourceMeta", 
        href = "http://creativecommons.org/publicdomain/zero/1.0/",
        rel = "cc:license",
        'xsi:type'="ResourceMeta")
  else
    new("meta", 
        content=rights,
        datatype="xsd:string",
        property="dc:rights",
        'xsi:type'="LiteralMeta")

}




