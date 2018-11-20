#' new with namespaced class name
#'
#' Convenience function for [methods::new()] that ensures that the provided
#' class name is namespaced with a package name.
#'
#' If the provided class name is not already namespaced (see
#' [methods::packageSlot()]), it will be namespaced with this package. This
#' mechanism is used by `new()` to disambiguate if the class name clashes
#' with a class defined in another package.
#' @note This may not completely eliminate messages on standard error about
#'   classes with the same name having been found in different packages. If
#'   they appear, they will most likely have come from the call to the
#'   [methods::initialize()] generic that `new()` issues at the end.
#' @param Class the name of the S4 class to be instantiated
#' @param ... additional parameters for [methods::new()]
#' @importFrom methods new className packageSlot
New <- function(Class, ...) {
  if (is.null(packageSlot(Class))) Class <- className(Class, "RNeXML")
  methods::new(Class, ...)
}

nexml.tree <- function(...) New("tree", ...)
