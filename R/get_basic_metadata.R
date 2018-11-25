#' Get citation from metadata
#' 
#' Extracts the citation annotation from the metadata annotation of the`nexml`
#' object, and returns its value.
#'
#' Currently the implementation looks for `dcterms:bibliographicCitation`
#' annotations. (Note that these may be given with any prefix in the metadata
#' so long as they expand to the same full property URIs.)
#' @seealso [get_metadata_values()]
#' @param nexml a nexml object
#' @return the citation if the metadata provides one that is non-empty, and
#'   NA otherwise. If multiple non-empty annotations are found, only the first
#'   one is returned.
#' @export
get_citation <- function(nexml){
  cit <- get_metadata_values(nexml, props = c("dcterms:bibliographicCitation"))
  if (length(cit) > 0) {
    # remove empty values
    cit[sapply(cit, length) > 0][1]
  } else
    NA
}

#' Get license from metadata
#'
#' Extracts the license annotation from the metadata annotation of the`nexml`
#' object, and returns its value.
#'
#' Currently the implementation looks for `cc:license` and `dc:rights`
#' annotations. (Note that these may be given with any prefix in the metadata
#' so long as they expand to the same full property URIs.)
#' @seealso [get_metadata_values()]
#' @param nexml a nexml object
#' @return the license if the metadata asserts one that is non-empty, and
#'   NA otherwise.If multiple non-empty annotations are found, only the first
#'   one is returned.
#' @export 
get_license <- function(nexml){
  lic <- get_metadata_values(nexml, props = c("dc:rights", "cc:license"))
  if (length(lic) > 0) {
    # remove empty values
    lic[sapply(lic, length) > 0][1]
  } else
    NA
}
