test_that('adj', {
  plot(0, 0)
  for (hadj in c(0, 1)) {
    for (padj in c(0, 1)) {
      colorbar(0.5, 0.5, horizontal = TRUE, box_adj = c(hadj, padj))
    }
  }
})
test_that('x, y, and horizontal', {
  plot(0, 0)
  colorbar(0, 1, horizontal = TRUE, box_adj = c(0, 1))
  colorbar(1, 0, horizontal = FALSE, box_adj = c(1, 0))
})

