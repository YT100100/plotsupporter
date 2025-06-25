test_that('adj', {

  plot(0, 0)
  for (hadj in c(0, 1)) {
    for (padj in c(0, 1)) {
      colorbar(0.5, 0.5, horizontal = TRUE, box_adj = c(hadj, padj),
               axis_lim = c(-2, 2), axis_at = -2:2)
    }
  }

})
test_that('x, y, and horizontal', {

  old_par <- par(no.readonly = TRUE)
  par(mar = c(5, 4, 4, 4) + 0.1)
  plot(0, 0)
  colorbar(0, 1, horizontal = TRUE, box_adj = c(0, 1),
           axis_lim = c(-2, 2), axis_at = -2:2)
  colorbar(0, 1, horizontal = TRUE, box_adj = c(0, 0),
           axis_lim = c(-2, 2), axis_at = -2:2)
  colorbar(1, 0, horizontal = FALSE, box_adj = c(1, 0),
           axis_lim = c(-2, 2), axis_at = -2:2)
  colorbar(1, 0, horizontal = FALSE, box_adj = c(0, 0),
           axis_lim = c(-2, 2), axis_at = -2:2)
  par(old_par)

})
test_that('axis_at', {

  plot(0, 0)
  for (i in seq(1, 9, length.out = 4)) {
    at <- seq(-2, 2, length.out = i)
    colorbar(0.5, i / 10, bar_width = 10,
             axis_lim = c(-2, 2), axis_at = at, axis_label = round(at, 1))
  }

})
test_that('axis_position', {

  plot(0, 0)
  colorbar(0.3, 0.5, axis_lim = c(-2, 2), axis_at = -2:2,
           horizontal = TRUE, axis_position = 'top', box_adj = c(0.5, 1))
  colorbar(0.3, 0.5, axis_lim = c(-2, 2), axis_at = -2:2,
           horizontal = TRUE, axis_position = 'bottom', box_adj = c(0.5, 0))
  colorbar(0.7, 0.5, axis_lim = c(-2, 2), axis_at = -2:2,
           horizontal = FALSE, axis_position = 'left', box_adj = c(0, 0.5))
  colorbar(0.7, 0.5, axis_lim = c(-2, 2), axis_at = -2:2,
           horizontal = FALSE, axis_position = 'right', box_adj = c(1, 0.5))

})
test_that('axislab', {

  plot(0, 0)

})

