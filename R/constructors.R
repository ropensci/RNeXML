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

#' Constructor for the respective class
#'
#' Creates an instance of the class corresponding to the respective NeXML
#' element, and initializes its slots with the provided parameters, if any.
#'
#' Usually, users won't need to invoke this directly.
#' @param ... optionally, parameters passed on to [`new()`][methods::new()]
#' @rdname constructors
#' @export
nexml.tree <- function(...) New("tree", ...)

#' @rdname constructors
#' @export
nexml.trees <- function(...) New("trees", ...)

#' @rdname constructors
#' @export
nexml.node <- function(...) New("node", ...)

#' @rdname constructors
#' @export
nexml.edge <- function(...) New("edge", ...)

#' @rdname constructors
#' @export
nexml.otu <- function(...) New("otu", ...)

#' @rdname constructors
#' @export
nexml.otus <- function(...) New("otus", ...)

#' @rdname constructors
#' @export
nexml.char <- function(...) New("char", ...)

#' @rdname constructors
#' @export
nexml.characters <- function(...) New("characters", ...)

#' @rdname constructors
#' @export
nexml.format <- function(...) New("format", ...)

#' @rdname constructors
#' @export
nexml.state <- function(...) New("state", ...)

#' @rdname constructors
#' @export
nexml.uncertain_state <- function(...) New("uncertain_state", ...)

#' @rdname constructors
#' @export
nexml.states <- function(...) New("states", ...)

#' @rdname constructors
#' @export
nexml.uncertain_states <- function(...) New("uncertain_state_set", ...)

#' @rdname constructors
#' @export
nexml.polymorphic_states <- function(...) New("polymorphic_state_set", ...)

#' @rdname constructors
#' @export
nexml.member <- function(...) New("member", ...)

#' @rdname constructors
#' @export
nexml.matrix <- function(...) New("obsmatrix", ...)

#' @rdname constructors
#' @export
nexml.row <- function(...) New("row", ...)

#' @rdname constructors
#' @export
nexml.seq <- function(...) New("seq", ...)

#' @rdname constructors
#' @export
nexml.cell <- function(...) New("cell", ...)
