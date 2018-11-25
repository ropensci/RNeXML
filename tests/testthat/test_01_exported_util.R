context("expored utility functions")

test_that("expand_prefix() works correctly", {
  uris <- c("license", "title")
  # returns unchanged if no namespaces
  testthat::expect_equal(expand_prefix(uris), uris)
  # returns unchanged if no matching namespace
  testthat::expect_equal(expand_prefix(uris, c(ns = "//foo/")), uris)
  uris2 <- paste0("foo:", uris)
  testthat::expect_equal(expand_prefix(uris2, c(ns = "//foo/")), uris2)
  # if none are prefixed and we give a base namespace, it's the same as paste0
  testthat::expect_equal(expand_prefix(uris, c("//foo/")), paste0("//foo/", uris))
  # should be able to handle NAs in the input
  uris <- c(NA, uris, NA)
  testthat::expect_equal(expand_prefix(uris), uris)
  testthat::expect_equal(expand_prefix(uris, c(ns = "//foo/")), uris)
  # not quite the same as paste0 if NAs are to be allowed
  testthat::expect_equal(expand_prefix(uris, c("//foo/")),
                         ifelse(is.na(uris), uris, paste0("//foo/", uris)))
  # namespaces get expanded 
  uris <- c("cc:license", "dc:title", "/nexml", "foo:bar", NA)
  nex <- new("nexml")
  expanded <- expand_prefix(uris, namespaces = nex@namespaces)
  testthat::expect_equal(expanded[1],
                         sub("^cc:", nex@namespaces["cc"], expanded[1]))
  testthat::expect_equal(expanded[2],
                         sub("^dc:", nex@namespaces["dc"], expanded[2]))
  testthat::expect_equal(expanded[3],
                         paste0(nex@namespaces["nex"], uris[3]))
  testthat::expect_equal(expanded[4], "foo:bar")
  testthat::expect_length(expanded, length(uris))
  testthat::expect_true(is.na(expanded[5]))
  # exact prefix doesn't matter so long as they expand to the same URI
  testthat::expect_equivalent(expand_prefix("ns1:foo", c(ns1 = "http://example.org/")),
                              expand_prefix("ns2:foo", c(ns2 = "http://example.org/")))
})
