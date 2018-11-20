context("internal utility functions")

test_that("lcapply() works correctly", {
  l <- list(a = "foo", b = "bar", c = NULL, NULL, "baz", d = "foobar")
  l.ans1 <- lapply(l, function(x) x)
  l.ans2 <- lcapply(l, function(x) x)
  testthat::expect_length(l.ans1, length(l))
  testthat::expect_length(l.ans2, length(l) - 2)
  testthat::expect_equal(names(l.ans1), names(l))
  testthat::expect_equal(names(l.ans2), names(l[c(1,2,5,6)]))
  testthat::expect_false("c" %in% names(l.ans2))
  testthat::expect_true("" %in% names(l.ans2))
  testthat::expect_equal(l.ans2[names(l.ans2) == ""][[1]], "baz")
})

test_that("findNextMethod() works correctly", {
  pkgEnv <- asNamespace("RNeXML")
  mdef <- selectMethod("toNeXML",
                       c("trees", "XMLInternalElementNode"),
                       optional = TRUE)
  testthat::expect_is(mdef, "MethodDefinition")
  mdef.next <- findNextMethod(mdef, envir = pkgEnv)
  testthat::expect_is(mdef.next, "MethodDefinition")
  testthat::expect_equal(mdef@defined, mdef.next@target)
  testthat::expect_equal(mdef@generic, mdef.next@generic)
  o.mm <- new(className(mdef@defined@.Data[1], mdef@defined@package[1]))
  testthat::expect_true(is(o.mm, mdef.next@defined@.Data[1]))
  mwn1 <- .methodWithNext(method = mdef, nextMethod = mdef.next)
  mwn2 <- addNextMethod(mdef, f = mdef@generic, envir = pkgEnv)
  testthat::expect_is(mwn1, "MethodWithNext")
  testthat::expect_is(mwn1@nextMethod, "MethodDefinition")
  testthat::expect_equal(mwn1@generic, mwn2@generic)
  testthat::expect_equal(mwn1@target, mwn2@target)
  testthat::expect_equal(mwn1@defined, mwn2@defined)
  testthat::expect_equal(mwn1@excluded, mwn2@excluded)
  testthat::expect_equal(mwn1@nextMethod@generic, mwn2@nextMethod@generic)
  testthat::expect_equal(mwn1@nextMethod@.Data, mwn2@nextMethod@.Data)
  # ignore attributes for the following two - package is in its own slot
  # of the signature object anyway, which will be part of the test
  testthat::expect_equivalent(mwn1@nextMethod@target, mwn2@nextMethod@target)
  testthat::expect_equivalent(mwn1@nextMethod@defined, mwn2@nextMethod@defined)
})
