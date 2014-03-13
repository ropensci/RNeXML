
#' publish nexml files to the web and receive a DOI
#' 
#' publish nexml files to the web and receive a DOI
#' @param nexml a nexml object (or file path)
#' @export 
#' @import rfigshare
nexml_publish <- function(nexml, ..., repository="figshare"){
  repository = match.arg(repository)
  switch(repository, 
         figshare = nexml_figshare(nexml, ...))
}


nexml_figshare <- function(nexml,
                           file = "nexml.xml", 
                           categories = "Evolutionary Biology", 
                           tags = c("phylogeny", "NeXML"),
                           visibility = "public", 
                           ...){



  m <- get_metadata(nexml)


  id <- fs_create(title = m["dc:title"],
                  description = m["dc:description"], 
                  type = "dataset")
  fs_add_authors(id, authors = m["dc:creator"])
  fs_add_categories(id, categories)
  fs_add_tags(id, tags)

  # Use object DOI instead of figshare id when available? 
  # Construct DOI from figshare id? 
  nexml <- add_meta(meta("dc:identifier", id), nexml)

  nexml_write(nexml, file) 

  fs_upload(id, file)
  if (visibility == "private") 
      fs_make_private(article_id, session)
  if (visibility == "public") 
      fs_make_public(article_id, session)

                     
  id

}
