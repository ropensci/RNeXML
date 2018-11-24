## code for better dealing with namespace-prefixed strings (generally URIs)

#' Expand namespace-prefixed string
#'
#' Substitutes the namespace prefix in the input vector of strings with
#' the corresponding namespaces.
#'
#' Namespace prefixes are expected to be separated by one or more semicolons.
#' Prefixes that cannot be matched to the vector of namespaces will be left
#' as is. For strings that do not have a namespace prefix, the vector of
#' namespaces can contain a base namespace, identified as not having a name,
#' with which these strings will be expanded.
#' @param x a character vector of potentially namespace-prefixed strings
#' @param namespaces a named vector of namespaces, with namespace prefixes
#'   being the names. A "base" namespace with an empty name can be included.
#'   If not provided, or if empty, the input vector is returned as is.
#' @return a character vector, of the same length as the input vector
#' @export
#' @importFrom stringi stri_match_first_regex stri_replace_first_regex
expand_prefix <- function(x, namespaces = NULL) {
  if (is.null(namespaces) || length(namespaces) == 0)
    x
  else {
    nsNames <- names(namespaces)
    if (is.null(nsNames))
      # single non-named element (or all elements unnamed)
      isBase <- rep(TRUE, length(namespaces))
    else
      isBase <- nsNames == ""
    # We're being clever here: if the prefix pattern doesn't match, we assign
    # the pattern for matching at the beginning of the string, and to have that
    # matched as all other namespaces we'll make it the name of the base
    # namespace, using an empty string for expansion if none was provided.
    if (any(isBase))
      names(namespaces)[isBase] <- "^"
    else
      namespaces <- c(namespaces, "^" = "")
    prefixes <- stringi::stri_match_first_regex(x, "^([^:]*):", cg_missing = "^")
    ns <- namespaces[match(prefixes[,2], names(namespaces))]
    if (all(is.na(ns)))
      x
    else {
      expanded <- stringi::stri_replace_first_regex(x, prefixes[,1], ns)
      ifelse(is.na(expanded), x, expanded)
    }
  }
}
