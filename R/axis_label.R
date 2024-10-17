#' Add axis labels with rotation
#'
#' @param side An integer specifying which side of the plot
#'   the axis is to be shown.
#'   Equivalent to \code{side} argument in \link{axis}.
#' @param at The points at which labels are to be drawn.
#'   Passing values of \link{axis} is possible.
#' @param labels Equivalent to \code{labels} argument in \link{axis} function.
#' @param type An integer either \code{'long'}, \code{'short'}
#'   or their abbreviations. It determines the center position of labels
#'   when \code{hadj} and \code{padj} are not given.
#' @param srt An angle for label rotation (string rotation).
#' @param font An integer indicating font of labels.
#' @param col An integer or a string indicating color of labels.
#' @param hadj An integer indicating adjustment (see par("adj"))
#'   for all labels parallel (‘horizontal’) to the reading direction.
#' @param padj An integer indicating adjustment (see par("adj"))
#'   for all labels perpendicular to the reading direction.
#'
#' @examples
#' data('iris')
#' b <- boxplot(Sepal.Length ~ Species, data = iris, xaxt = 'n')
#' at <- axis(side = 1, at = seq_along(b$names), labels = FALSE)
#' axis_label(side = 1, at = at, labels = b$names, srt = 45)
#'
#' @export
axis_label <- function(
    side, at, labels = at, type = 'long', line = NA, srt = NA, font = NA,
    col = NULL, hadj = NA, padj = NA, ...) {

  # check arguments ============================================================
  # arguments with length 1
  for (arg_name in c('side', 'type', 'line', 'srt', 'font', 'hadj', 'padj')) {
    arg <- get(arg_name)
    if (length(arg) >= 2) {
      msg <- sprintf(
        'Multiple values were given as `%s`. Only the first value is used.',
        arg_name)
      warning(msg)
    }
    assign(arg_name, arg[1])
  }

  # side
  side <- as.integer(side)
  stopifnot(side %in% 1:4)

  # at
  at <- as.numeric(at)

  # type
  types <- c('short', 'long')
  type <- types[pmatch(type, types)]
  if (is.na(type)) stop('Invalid `type`.')

  # labels
  if (!length(labels) %in% c(1, length(at))) {
    warning('Length of `labels` is neither 1 nor length of `at`.')
  }
  labels <- as.character(labels)

  # line
  line <- as.numeric(line)

  # srt
  srt <- as.numeric(srt)

  # font
  font <- as.integer(font)
  stopifnot(font %in% c(1:5, NA))

  # hadj, padj
  hadj <- as.numeric(hadj)
  padj <- as.numeric(padj)

  # determine graphic parameters ===============================================
  is_horizontal <- side %in% c(1, 3)

  # label rotation
  if (is.na(srt)) srt <- if (is_horizontal) 0 else 90

  # gap between axis and labels
  if (is.na(line)) {
    if (is_horizontal) {
      line <- if (type == 'short') 1 else 0.5
    } else {
      line <- if (type == 'short') 1 else 0.5
    }
  }

  # label coordinate
  if (is_horizontal) {
    x <- at
    y <- if (side == 1) {
      par('usr')[3] - par('cxy')[2] * line
    } else {
      par('usr')[4] + par('cxy')[2] * line
    }
  } else {
    y <- at
    x <- if (side == 2) {
      par('usr')[1] - par('cxy')[1] * line
    } else {
      par('usr')[2] + par('cxy')[1] * line
    }
  }

  # label position adjustment
  if (type == 'short') {
    hadj <- padj <- 0.5
  } else {
    sin_srt <- sin(srt / 180 * pi)
    cos_srt <- cos(srt / 180 * pi)
    th <- sin(pi / 8)
    if (side == 1) {
      hadj <- if (sin_srt < -th) 0 else if (sin_srt < th) 0.5 else 1
      padj <- if (cos_srt < -th) 0 else if (cos_srt < th) 0.5 else 1
    } else if (side == 2) {
      hadj <- if (cos_srt < -th) 0 else if (cos_srt < th) 0.5 else 1
      padj <- if (sin_srt < -th) 1 else if (sin_srt < th) 0.5 else 0
    } else if (side == 3) {
      hadj <- if (sin_srt < -th) 1 else if (sin_srt < th) 0.5 else 0
      padj <- if (cos_srt < -th) 1 else if (cos_srt < th) 0.5 else 0
    } else if (side == 4) {
      hadj <- if (cos_srt < -th) 1 else if (cos_srt < th) 0.5 else 0
      padj <- if (sin_srt < -th) 0 else if (sin_srt < th) 0.5 else 1
    }
  }

  # draw axis label ============================================================
  old_xpd <- par('xpd')
  par(xpd = TRUE)
  text(x = x, y = y, labels = labels, srt = srt, font = font, col = col,
       adj = c(hadj, padj), ...)
  par(xpd = old_xpd)

}
