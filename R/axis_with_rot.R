#' Convert p-value to significance mark
#'
#' Add x-axis with vertical labels
#'
#' @param at Equivalent to `at` argument in `axis()` function.
#' @param labels Equivalent to `labels` argument in `axis()` function.
#' @param srt An angle for label rotation.
#' @param pos A parameter to adjust vertical positions of labels.
#' @param adj A parameter to adjust relationship between coordinates of labels
#' and actual positions of labels.
#'
#' @export
xaxis_with_vert_label <- function(
    at, labels, srt = 90, pos = 0.05, adj = c(1, 0.5)) {

  stopifnot(length(at) == length(labels))

  usr <- par()$usr
  y <- sum(usr[3:4] * (1:0 + pos * c(1, -1)))

  par(xpd = TRUE)
  axis(1, at = at, labels = rep('', length(labels)))
  text(at, y, labels, srt = srt, adj = adj)
  par(xpd = FALSE)

}

axis_srt <- function(
    side, at = NULL, labels = TRUE, srt = NULL, tick = TRUE, line = NA,
    pos = NA, outer = FALSE, font = NA, lty = 'solid',
    lwd = 1, lwd.ticks = lwd, col = NULL, col.ticks = NULL,
    hadj = NA, padj = NA, gap.axis = NA, ...) {

  # check arguments
  stopifnot(length(side) == 1)

  # if srt is not given or labels == FALSEs,
  # this function works as a standard `axis()`
  if (is.null(srt) | identical(labels, FALSE)) {
    return(axis(side, at, labels, tick, line, pos, outer, font, lty, lwd,
                lwd.ticks, col, col.ticks, hadj, padj, gap.axis, ...))
  }

  # draw axis without labels
  at <- axis(
    side, at, labels = FALSE, tick, line, pos, outer, font, lty,
    lwd, lwd.ticks, col, col.ticks, hadj, padj, gap.axis, ...)

  # determine coordinate of labels
  # yをどう決めるか？
  labloc <- aaaa

  # add text
  if (identical(side, 1)) {

    if (srt < 45) {
      if (identical(hadj, NA)) hadj <- 0
      if (identical(padj, NA)) padj <- 0
    }
    if (identical(labels, TRUE)) labels <- as.character(at)
    text(x = at, y = labloc, labels = labels)

  }
  stopifnot(length(srt) == 1)

  # if (is.null(srt)) {
  #   srt <-
  # }


}

plot(0, xaxt = 'n', yaxt = 'n')
axis_srt(1)
axis_srt(2)
axis_srt(3)
axis_srt(4)
