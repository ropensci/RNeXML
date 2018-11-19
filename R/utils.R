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
