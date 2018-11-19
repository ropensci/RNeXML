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