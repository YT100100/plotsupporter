dat <- expand.grid(x1 = factor(letters[1:5]),
                   x2 = factor(LETTERS[1:3]),
                   rep = 1:10)
set.seed(1)
dat <- within(dat, {
  y <- as.integer(x1) + as.integer(x2) * 3 + rnorm(nrow(dat), sd = 2)
})

test_that('standard', {

  # debugonce(easy_barplot)

  # デフォルト設定
  easy_barplot(x1 = dat$x1, y = dat$y)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'tukey')
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'dunnett')

  # 点を表示しない場合
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_point = FALSE)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_point = FALSE, test_method = 'tukey')
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_point = FALSE, test_method = 'dunnett')

  # エラーバーを表示しない場合
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_errorbar = FALSE)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_errorbar = FALSE, test_method = 'tukey')
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, show_errorbar = FALSE, test_method = 'dunnett')

  # タイトルなど


})
test_that('bar options', {

  # debugonce(easy_barplot)

  easy_barplot(
    x1 = dat$x1, y = dat$y,
    bar_space_x1 = 0.2, bar_space_x2 = 2,
    bar_col = 'green', bar_border = 'red', bar_lwd = 2)

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y,
    bar_space_x1 = 0.2, bar_space_x2 = 2,
    bar_col = RColorBrewer::brewer.pal(3, 'Greens'),
    bar_border = 'red', bar_lwd = 2)

})
test_that('error bars', {

  # debugonce(easy_barplot)

  easy_barplot(
    x1 = dat$x1, y = dat$y,
    errorbar_length = 1, errorbar_col = 'orange', errorbar_lwd = 3)

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y,
    errorbar_length = 1, errorbar_col = 'orange', errorbar_lwd = 3)

})
test_that('axis labels', {

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y,
    main = 'Main', xlab = 'Groups',
    ylab = expression(test * phantom(n) * (unit)))

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y, main = 'Main',
    main_line = 0, main_cex = 3, main_font = 3, main_col = 'green')

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y, xlab = 'Groups',
    xlab_line = 1, xlab_cex = 3, xlab_font = 3, xlab_col = 'green')

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y,
    ylab = expression(test * phantom(n) * (unit)),
    ylab_line = 0, ylab_cex = 3, ylab_font = 3, ylab_col = 'green')

})
test_that('long labels', {

  # dat <- expand.grid(x1 = factor(letters[1:5]),
  #                    x2 = factor(LETTERS[1:3]),
  #                    rep = 1:10)
  # dat <- within(dat, {
  #   y <- as.integer(x1) + as.integer(x2) * 3 + rnorm(nrow(dat), sd = 2)
  # })
  # # debugonce(easy_barplot)
  # easy_barplot(x1 = dat$x1, y = dat$y)
  # easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y)


})
test_that('including negative y', {


  # point_at_top = FALSE

  # dat <- expand.grid(x1 = factor(letters[1:5]),
  #                    x2 = factor(LETTERS[1:3]),
  #                    rep = 1:10)
  # dat <- within(dat, {
  #   y <- as.integer(x1) + as.integer(x2) * 3 + rnorm(nrow(dat), sd = 2)
  # })
  # # debugonce(easy_barplot)
  # easy_barplot(x1 = dat$x1, y = dat$y)
  # easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y)


})
