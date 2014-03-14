
#' publish nexml files to the web and receive a DOI
#' 
#' publish nexml files to the web and receive a DOI
#' @param nexml a nexml object (or file path)
#' @param ... additional arguments, depending on repository. See examples.
#' @param repository desitination respository
#' @return a digital object identifier to the published data
#' @export 
nexml_publish <- function(nexml, ..., repository="figshare"){
  repository = match.arg(repository)
  switch(repository, 
         figshare = nexml_figshare(nexml, ...))
}


#' publish nexml to figshare
#'
#' publish nexml to figshare
#' @param nexml a nexml object (or file path to a nexml file)
#' @param file The filename desired for the object, if nexml is not already a file.
#'  if the first argument is already a path, this value is ignored.  
#' @param categories The figshare categories, must match available set. see \code{fs_add_categories}
#' @param tags Any keyword tags you want to add to the data.  
#' @param visibility whether the results should be published (public), or kept private,
#'  or kept as a draft for further editing before publication.  (New versions can be updated,
#'  but any former versions that was once made public will always be archived and cannot be removed).  
#' @param id an existing figshare id (e.g. from fs_create), to which this file can be appended.  
#' @param ... additional arguments
#' @return a digital object identifier (doi) to the published data. (DOI will be reserved but not
#'  active for private or draft publications) 
#' @export
nexml_figshare <- function(nexml,
                           file = "nexml.xml", 
                           categories = "Evolutionary Biology", 
                           tags = c("phylogeny", "NeXML"),
                           visibility = c("public", "private", "draft"),
                           id = NULL, 
                           ...){

  visibility = match.arg(visibility)

  success <- require(rfigshare)
  if(!success){
    message("rfigshare package not found. Attempting to install")
    install.packages("rfigshare")
    success <- require(rfigshare)
     if(!success)  
      stop("The rfigshare package must be installed to publish data to figshare")
  }



  # handle nexml as a file path or as an object 
  if(file.exists(nexml)){
    file <- nexml
    nexml <- nexml_read(nexml)
  }
 
  m <- get_metadata(nexml)

  if(!is.null(id)){
    id <- fs_create(title = m["dc:title"],
                    description = m["dc:description"], 
                    type = "dataset")
  }
  doi <- paste("doi:10.6084/m9.figshare", id, sep=".")

  fs_add_authors(id, authors = m["dc:creator"])
  fs_add_categories(id, categories)
  fs_add_tags(id, tags)

  # Use object DOI instead of figshare id when available? 
  # Construct DOI from figshare id? 
  nexml <- add_meta(meta("dc:identifier", doi), nexml)

  nexml_write(nexml, file) 

  fs_upload(id, file)
  if (visibility == "private"){ 
      fs_make_private(id, session)
      message(paste0("Your data has been uploaded to figshare privately.
           You may make further edits and publish the data from
           the online control panel at figshare.com or by using 
           the rfigshare package and the article_id:", id, ". Your 
           doi has been reserved but will not resolve until the article 
           is made public."))

  }
  if (visibility == "public"){ 
      fs_make_public(id, session)
      message(paste0("Your data is published and now accessible at doi:", doi))
  } else {
  message(paste0("Your data has been uploaded to figshare as a draft.
           You may make further edits and publish the data from
           the online control panel at figshare.com or by using 
           the rfigshare package and the article_id:", id, ". Your 
           doi has been reserved but will not resolve until the article 
           is made public."))
  }

  doi 
}
