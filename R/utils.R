## utility functions, presumably used mostly or entirely internally

#' Compact list then lapply
#'
#' Compacts the list (i.e., removes NULL objects), then calls [`lapply()`][base::lapply()]
#' on the result with the remaining parameters.
#' @param X the list object
#' @param ... remaining arguments to `lapply()`
#' @importFrom plyr compact
lcapply <- function(X, ...) {
  X <- plyr::compact(X)
  lapply(X, ...)
}

#' Front-end to dplyr::coalesce to deal with NULL vectors
#'
#' Replaces any NULL argument with a vector of `NA`, and casts every vector
#' to the same type as the last vector. After that, calls [dplyr::coalesce()].
#' @param ... the vectors to coalesce on NA
#' @return a vector of the same type and length as the last argument
#' @seealso [dplyr::coalesce()]
#' @importFrom dplyr coalesce
coalesce_ <- function(...) {
  idvecs <- list(...)
  idvecs <- lapply(idvecs,
                   function(v, template) {
                     if (is.null(v))
                       as(rep(NA, times=length(template)), typeof(template))
                     else
                       as(v, typeof(template))
                   },
                   template = idvecs[[length(idvecs)]])
  dplyr::coalesce(!!!idvecs)
}

#' Treats zero-length character vectors as empty strings
#'
#' If the argument is a zero-length character vector (character(0)), returns
#' an empty string (which is a character vector of length 1). Otherwise passes
#' through the argument.
#' @param x the object to be tested for zero-length character vector
#' @return an empty string if `x` is a character vector of length zero, and `x`
#'   otherwise
charzero_as_empty <- function(x) {
  if (is.character(x) && length(x) == 0)
    ""
  else
    x
}