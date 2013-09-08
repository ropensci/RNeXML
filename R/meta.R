## Utilities for adding additional metadata

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




