test_that('basic use', {

  plot(1:10, 1:10, xaxt = 'n', yaxt = 'n')
  for (side in 1:4) {
    at <- axis(side, labels = FALSE)
    axis_label(side, at = at)
    axis_label(side, at = at, line = 2, col = 'red', font = 3)
  }

})
test_that('automatic string rotation', {

  for (srt in seq(0, 360, 10)) {
    plot(1:10, 1:10, xaxt = 'n', yaxt = 'n', main = paste('srt =', srt))
    for (side in 1:4) {
      at <- axis(side, labels = FALSE)
      labels <- sapply(at, function(a) paste(rep(a, 7), collapse = ''))
      axis_label(side, at = at, labels = labels, srt = srt)
    }
    Sys.sleep(1)
  }

})
test_that('manual adjustment of locations', {

  plot(1:10, 1:10, xaxt = 'n', yaxt = 'n')
  for (side in 1:4) {
    at <- axis(side, labels = FALSE)
    labels <- sapply(at, function(a) paste(rep(a, 4), collapse = ''))
    axis_label(side, at = at, labels = labels, hadj = 0.7)
  }

  plot(1:10, 1:10, xaxt = 'n', yaxt = 'n')
  for (side in 1:4) {
    at <- axis(side, labels = FALSE)
    labels <- sapply(at, function(a) paste(rep(a, 4), collapse = ''))
    axis_label(side, at = at, labels = labels, padj = 0.7)
  }

})
