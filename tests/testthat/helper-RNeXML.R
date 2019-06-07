#' @importFrom rlang enquo
expect_true_or_null <- function(o){
  val <- rlang::enquo(o)
  if(!is.null(o)){
    testthat::expect_true(!!val)
  } else {
    testthat::expect_null(!!val)
  }
}


library("XML")