#' Convert p-value to significance mark
#'
#' @param p A numeric vector of p-values.
#' @return Characters either \code{"n.s."}, \code{"*"}, \code{"**"}, or \code{"***"},
#'     if \eqn{p > 0.05}, \eqn{p <= 0.05}, \eqn{p <= 0.01}, or \eqn{p <= 0.001},
#'     respectively.
#' @export

convert_pval_to_txt <- function(p) {
  ans <- vector(length = length(p))
  ans[p > 0.05]   <- 'n.s.'
  ans[p <= 0.05]  <- '*'
  ans[p <= 0.01]  <- '**'
  ans[p <= 0.001] <- '***'
  ans
}

