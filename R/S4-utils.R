## utilities to better deal with S4 classes and generics

#' Finds the method that callNextMethod() should chain to
#'
#' Attempts to find the "next" method in the inheritance chain. This would
#' (ideally) be the method that `methods::callNextMethod()` would chain to,
#' as a result of the method `methods::addNextMethod()` would find (and return
#' in the `nextMethod` slot of the `MethodWithNext`
#' object). Hence, in theory one shouldn't ever need this, but unfortunately
#' `addNextMethod()` is broken (and errors out) if one of the classes in the
#' signature name-clashes with an S4 class defined in another package that is
#' loaded.
#'
#' The next method will be determined by the S4 inheritance chain. However,
#' this function will walk only the inheritance chain of those arguments in
#' the signature that are defined in the package of the generic method from
#' which this function was invoked (directly or indirectly). If there are
#' no such parameters in the signature, or if there is more than one,
#' finding the next method is handed off to `methods::addNextMethod()`.
#' @note In theory a class name clash between packages shouldn't be a problem
#'   because class names can be namespaced, and the `MethodDefinition`
#'   object passed to `addNextMethod()` has all the necessary namespace
#'   information. Hopefully, at some point this gets fixed in R, and then we
#'   don't need this anymore.
#' @param method `MethodDefinition`, the method for which to find
#'   the next method
#' @param f `standardGeneric`, the standard generic for which to find
#'   the next method. By default this will be obtained from `method`.
#' @param envir the environment in which to find the method
#' @return a `MethodDefinition` object that is the next method in the
#'   chain by inheritance
#' @importFrom methods getClassDef selectMethod addNextMethod
#' @importFrom utils packageName
findNextMethod <- function(method, f = NULL, envir = topenv()) {
  if (is(method, "MethodWithNext"))
    method@nextMethod
  else {
    if (is.null(f)) f <- method@generic
    sigDefined <- method@defined
    nextMethod <- NULL
    ourPkg <- packageName(env = envir)
    if (is.null(ourPkg)) {
      warning("failed to determine package where findNextMethod() was called")
      ourPkg <- "RNeXML"
    }
    isOurs <- sigDefined@package == ourPkg
    if (sum(isOurs) == 1) {
      clnames <- mapply(className,
                        sigDefined@.Data,
                        sigDefined@package,
                        SIMPLIFY = FALSE)
      names(clnames) <- sigDefined@names
      ourClass <- clnames[isOurs][[1]]
      super <- getClassDef(ourClass)@contains
      for (clExt in super) {
        superClassName <- className(clExt@superClass, clExt@package)
        sig <- clnames
        sig[[sigDefined@names[isOurs]]] <- superClassName
        nextMethod <- selectMethod(f, signature = sig, optional = TRUE)
        if (! is.null(nextMethod)) {
          nextMethod@target <- method@defined
          break
        }
      }
    }
    if (is.null(nextMethod)) {
      method <- addNextMethod(method, f, envir = envir)
      nextMethod <- method@nextMethod
    }
    nextMethod
  }
}

#' Saves the next method in the method meta data
#'
#' Promotes the given method definition to an instance of
#' `MethodWithNext`, thereby recording the next
#' method in the `nextMethod` slot.
#' @note `MethodWithNext` objects are normally returned by
#'   `methods::addNextMethod()`, but a constructor function for the class
#'   seems missing (or is undocumented?). This provides one.
#' @param method the `MethodDefinition` object to promote
#' @param nextMethod the `MethodDefinition`
#'   object to record as the next method
#' @param .cache whether to cache the promoted method definition object
#'   (using `methods::cacheMethod()`)
#' @return an instance of `MethodWithNext`,
#'   which has the next method in the `nextMethod` slot
#' @importFrom methods getGeneric cacheMethod
#' @importClassesFrom methods MethodWithNext
.methodWithNext <- function(method, nextMethod, .cache = FALSE) {
  methodWithNext <- new("MethodWithNext",
                        method,
                        nextMethod = nextMethod,
                        excluded = list(.sigLabel(method@defined)))
  if (.cache) {
    cacheMethod(method@generic,
                method@target,
                methodWithNext,
                fdef = getGeneric(method@generic),
                inherited = TRUE)
  }
  methodWithNext
}

#' Create a label for a method signature
#'
#' Creates a label for a signature mirroring the result of `.sigLabel()`
#' in the `methods` package, which unfortunately does not export the function.
#' This is needed, for example, for the `excluded` slot in the
#' `MethodWithNext` class.
#' @param signature the signature for which to create a label, as a vector
#'   or list of strings, or as an instance of `signature`.
#' @return a character string
.sigLabel <- function(signature) {
  if (is(signature, "signature")) signature <- signature@.Data
  paste(signature, collapse = "#")
}

#' Caches next method in the calling environment
#'
#' If the calling environment does not have the next method to be invoked
#' in the inheritance chain cached yet, this will find the next method
#' (using `findNextMethod()`, and cache it in the calling environment such
#' that a subsequent call to `methods::callNextMethod()` will find and use
#' it.
#'
#' As per the description, what this function does would normally already
#' be done by invoking `methods::callNextMethod()`, so in theory this should
#' be entirely redundant at best. However, `methods::addNextMethod()`, which
#' is invoked by `callNextMethod()` if a next method isn't cached yet, is
#' broken (errors out) if one of the classes in the signature name-clashes
#' with a class defined in another package. Calling this function prior to
#' `callNextMethod()` is meant to work around that.
.cacheNextMethod <- function() {
  callEnv <- parent.frame(1L)
  nextMethod <- callEnv$.nextMethod
  if (is.null(nextMethod)) {
    parent <- sys.parent(1)
    methodFun <- sys.function(parent)
    if (! is(methodFun, "MethodDefinition"))
      stop(".cacheNextMethod() not invoked from class method")
    method <- callEnv$.Method
    if (is.null(method)) method <- methodFun
    f <- callEnv$.Generic
    if (is.null(f)) f <- method@generic
    nextMethod <- findNextMethod(method, f = f, envir = callEnv)
    if (is.null(nextMethod)) stop("No next method available")
    assign(".nextMethod", nextMethod, envir = callEnv)
    assign(".Generic", f, envir = callEnv)
    if (is.null(callEnv$.Method)) {
      assign(".Method", 
             .methodWithNext(method = method, nextMethod = nextMethod),
             method,
             envir = callEnv)
    }
  }
}

#' Calls the given generic with the given arguments
#'
#' Calls the given generic with the given arguments, using the method
#' whose signature matches the arguments.
#'
#' Uses `methods::selectMethod()` to find the matching method. In theory,
#' this is at best wholly redundant with what standard S4 generics already
#' do by themselves. However, the generics dispatch for S4 seems (at least
#' currently) broken at least if the first argument in the signature is
#' a class that name-clashes with a class defined in another package. In
#' that case, whether the standard dispatch works correctly or not can depend
#' on `search()` order, and can change within a session
#' depending on the order in which packages are loaded.
#' @param f the generic, as a character string or a `standardGeneric`
#'   object
#' @param ... the arguments (named and/or unnamed) with which to call the
#'   matching method
#' @param .package the package name for finding the generic (if `f` is a character
#'   string); by default the package is determined from the calling environment
#' @return the value returned by the method
#' @importFrom methods getGeneric selectMethod
#' @importFrom utils packageName
.callGeneric <- function(f, ..., .package = NULL) {
  if (is.null(.package)) {
    .package <- packageName()
    if (is.null(.package)) {
      warning("failed to determine package where .callGeneric() was called")
      .package <- "RNeXML"
    }
  }
  where <- asNamespace(.package)
  if (!is(f, "standardGeneric")) f <- getGeneric(f, where = where)
  .Args <- list(...)
  sigArgs <- .Args[seq_along(f@signature)]
  sigClasses <- sapply(sigArgs, .classForSig, f)
  method <- selectMethod(f, sigClasses)
  method(...)
}

.classForSig <- function(sigArg, f = NULL) {
  clname <- class(sigArg)
  # multiple classes?
  if (length(clname) > 1) {
    # if the generic has a value class, try and match it
    if ((! is.null(f)) && length(f@valueClass) > 0) {
      isValueClass <- clname == f@valueClass
      if (any(isValueClass)) clname <- clname[isValueClass]
    }
    # if none match, or if there's no value class, take the first one
  }
  clname[1]
}
