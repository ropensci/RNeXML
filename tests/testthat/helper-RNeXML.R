#' @importFrom rlang enquo
expect_true_or_null <- function(o){
  val <- rlang::enquo(o)
  if(!is.null(o)){
    expect_true(!!val)
  } else {
    expect_null(!!val)
  }
}


library("XML")