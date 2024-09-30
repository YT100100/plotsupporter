#' Convert relative coordinates to absolute coordinates in plot region
#'
#' @export
plot_coord_x <- function(x) {
  w <- cbind(1 - x, x)
  apply(w, 1, weighted.mean, x = par()$usr[1:2])
}

#' Convert relative coordinates to absolute coordinates in plot region
#'
#' @export
plot_coord_y <- function(y) {
  w <- cbind(1 - y, y)
  apply(w, 1, weighted.mean, x = par()$usr[3:4])
}

