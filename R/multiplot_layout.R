#' Segmenting plot area beautifully
#'
#' @export
multiplot_layout <- function(mat,
                             upper_space = 0.05, lower_space = 0.05,
                             right_space = 0.05, left_space = 0.05, ...) {

  nc <- ncol(mat)
  nr <- nrow(mat)

  width_plot_ratio  <- 1 - right_space - left_space
  height_plot_ratio <- 1 - upper_space - lower_space

  widths_ratio  <- rep(width_plot_ratio  / nc, nc)
  heights_ratio <- rep(height_plot_ratio / nr, nr)

  widths_ratio [1]  <- widths_ratio [1]  + left_space
  widths_ratio [nc] <- widths_ratio [nc] + right_space
  heights_ratio[1]  <- heights_ratio[1]  + upper_space
  heights_ratio[nr] <- heights_ratio[nr] + lower_space

  width_cm  <- dev.size('cm')[1]
  height_cm <- dev.size('cm')[2]
  width_in  <- dev.size('in')[1]
  height_in <- dev.size('in')[2]

  widths_cm  <- widths_ratio  * width_cm
  heights_cm <- heights_ratio * height_cm

  layout(mat = mat,
         widths  = sapply(widths_cm , lcm),
         heights = sapply(heights_cm, lcm), ...)

  function(i, mai) {

    i_rc <- which(mat == i, arr.ind = TRUE)
    i_r <- i_rc[1, 'row']
    i_c <- i_rc[1, 'col']

    if (i_r == 1)  mai[3] <- mai[3] + height_in * upper_space
    if (i_r == nr) mai[1] <- mai[1] + height_in * lower_space
    if (i_c == 1)  mai[2] <- mai[2] + width_in  * left_space
    if (i_c == nc) mai[4] <- mai[4] + width_in  * right_space
    par(mai = mai)

  }

}
