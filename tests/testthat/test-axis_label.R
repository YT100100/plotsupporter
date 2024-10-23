test_that('basic use', {

  plot(1:10, 1:10, xaxt = 'n', yaxt = 'n')
  for (side in 1:4) {
    at <- axis(side, labels = FALSE)
    axis_label(side, at = at)
    axis_label(side, at = at, line = 2, col = 'red', font = 3)
  }

})
test_that('string rotation', {

  for (srt in seq(0, 360, 10)) {
    plot(1:10, 1:10, xaxt = 'n', yaxt = 'n', main = srt)
    for (side in 1:4) {
      at <- axis(side, labels = FALSE)
      labels <- sapply(at, function(a) paste(rep(a, 7), collapse = ''))
      axis_label(side, at = at, labels = labels, srt = srt)
    }
    Sys.sleep(1)
  }

})
