#' Convert p-value to significance mark
#'
#' @export
convert_pval_to_txt <- function(p) {
  ans <- vector(length = length(p))
  ans[p > 0.05]   <- 'n.s.'
  ans[p <= 0.05]  <- '*'
  ans[p <= 0.01]  <- '**'
  ans[p <= 0.001] <- '***'
  ans
}

