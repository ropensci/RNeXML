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

test_that(".classForSig() works correctly", {
  f1 <- getGeneric("toNeXML", package = "RNeXML")
  f2 <- getGeneric("fromNeXML", package = "RNeXML")
  obj <- nexml.tree()
  testthat::expect_equal(.classForSig(obj, f1), class(obj)[1])
  testthat::expect_equal(.classForSig(obj, f2), class(obj)[1])
  obj <- XML::newXMLNode("foo")
  testthat::expect_equal(.classForSig(obj, f1), "XMLInternalElementNode")
  testthat::expect_equal(.classForSig(obj, f2), "XMLInternalElementNode")
  class(obj) <- c("bogus", class(obj))
  testthat::expect_equal(.classForSig(obj, f1), class(obj)[2])
  testthat::expect_equal(.classForSig(obj, f2), class(obj)[1])
})

test_that(".callGeneric works correctly", {
  tr1 <- nexml.tree(id = "tr1",
                    node = New("ListOfnode", list(New("node", id = "n1"))))
  trblock <- New("trees", id = "trees1", tree = New("ListOftree", list(tr1)))
  xml1 <- as(trblock, "XMLInternalNode")
  xml2 <- .callGeneric("toNeXML", trblock, XML::newXMLNode("trees"), .package = "RNeXML")
  xml3 <- toNeXML(trblock, XML::newXMLNode("trees"))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml3, indent = FALSE))
  nexml1 <- as(xml1, "trees")
  nexml2 <- .callGeneric("fromNeXML", New("trees"), xml1, .package = "RNeXML")
  nexml3 <- fromNeXML(New("trees"), xml1)
  # roundtrip back to XML, using "as"
  xml1.1 <- as(nexml1, "XMLInternalNode")
  xml1.2 <- as(nexml2, "XMLInternalNode")
  xml1.3 <- as(nexml3, "XMLInternalNode")
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.1, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
  # roundtrip back to XML, using .callGeneric
  xml1.1 <- .callGeneric("toNeXML", nexml1, XML::newXMLNode("trees"), .package = "RNeXML")
  xml1.2 <- .callGeneric("toNeXML", nexml2, XML::newXMLNode("trees"), .package = "RNeXML")
  xml1.3 <- .callGeneric("toNeXML", nexml3, XML::newXMLNode("trees"), .package = "RNeXML")
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.1, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
  # roundtrip back to XML, using toNeXML generic dispatch
  xml1.1 <- toNeXML(nexml1, XML::newXMLNode("trees"))
  xml1.2 <- toNeXML(nexml2, XML::newXMLNode("trees"))
  xml1.3 <- toNeXML(nexml3, XML::newXMLNode("trees"))
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1.1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.1, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.2, indent = FALSE))
  testthat::expect_equal(XML::saveXML(xml1, indent = FALSE),
                         XML::saveXML(xml1.3, indent = FALSE))
})

test_that("coalesce_() works correctly", {
  dta <- data.frame(col1 = rep(NA, times = 5),
                    col2 = 1:5,
                    col3 = letters[1:5])
  dta[3:4, "col2"] <- NA
  dta[2:3, "col3"] <- NA
  last <- letters[6:10]
  # let's start off with some tests for the presence of the problems that
  # coalesce_ is trying to solve, because in their absence we won't need it
  #
  # (1) dplyr::coalesce is strict about type, and for some reason vectors
  # that are all NAs default to type logical
  testthat::expect_is(dta[, "col1"], "logical")
  testthat::expect_error(dplyr::coalesce(dta$col1, dta$col3, last))
  testthat::expect_error(dplyr::coalesce(dta$col3, last)) # col3 is a factor
  # (2) dplyr::coalesce doesn't gracefully deal with NULL arguments, which
  # can result from referencing column names that aren't there, requiring
  # a conditional testing for presence of the column
  testthat::expect_error(dplyr::coalesce(dta$col2, dta$foo, last))

  # now test that coalesce_ deals with these issues and gives correct results
  res <- ifelse(is.na(dta[,"col3"]), last, as.character(dta[,"col3"]))
  testthat::expect_is(coalesce_(dta$col1, dta$col3, last), "character")
  testthat::expect_false(any(is.na(coalesce_(dta$col1, dta$col3, last))))
  testthat::expect_equal(coalesce_(dta$col1, dta$col3, last), res)
  testthat::expect_equal(coalesce_(dta$col1, dta$col3, dta$foo, last), res)
  testthat::expect_equal(coalesce_(dta$col1, dta$col3, NULL, last), res)
  testthat::expect_is(coalesce_(dta$col1, dta$col2), "integer")
  testthat::expect_equal(sum(is.na(coalesce_(dta$col1, dta$col2))), 2)
  testthat::expect_false(any(is.na(coalesce_(dta$col1, dta$col2, 1:5))))
})
