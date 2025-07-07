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

})
test_that('no data in a group', {

  # debugonce(easy_barplot)

  dat1 <- subset(dat, x1 != 'a' | x2 != 'A')

  easy_barplot(x1 = dat1$x1, y = dat1$y)
  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y)
  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y, remove_level_without_data = TRUE)

  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y, test_method = 'tukey')
  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y, test_method = 'tukey', remove_level_without_data = TRUE)

  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y, test_method = 'dunnett')
  easy_barplot(x1 = dat1$x1, x2 = dat1$x2, y = dat1$y, test_method = 'dunnett', remove_level_without_data = TRUE)

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
test_that('label options', {

  # debugonce(easy_barplot)

  easy_barplot(
    x1 = dat$x1, y = dat$y, test_method = 'tukey',
    signif_label_cex = 3,
    signif_label_space = 0.2,
    signif_label_adj = c(1, 1),
    signif_label_col = 'red',
    signif_label_font = 2,
    signif_label_srt = 20)

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'tukey',
    signif_label_cex = 0.8,
    signif_label_space = 0.2,
    signif_label_adj = c(1, 1),
    signif_label_col = 'red',
    signif_label_font = 2,
    signif_label_srt = 20)

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'tukey',
    signif_label_at_top = FALSE)
  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = -dat$y, test_method = 'tukey',
    signif_label_at_top = FALSE)

})
test_that('error bar options', {

  # debugonce(easy_barplot)

  easy_barplot(
    x1 = dat$x1, y = dat$y,
    errorbar_length = 1, errorbar_col = 'orange', errorbar_lwd = 3)

  easy_barplot(
    x1 = dat$x1, x2 = dat$x2, y = dat$y,
    errorbar_length = 1, errorbar_col = 'orange', errorbar_lwd = 3)

})
test_that('axis label options', {


  # show_axis_label = c(TRUE, TRUE, TRUE),
  # axis_label_x1 = NULL,
  # axis_label_x2 = NULL,
  # axis_label_line = c(1, 2, 1),
  # axis_label_srt = c(90, 0, 90),
  # axis_label_font = c(1, 1, 1),
  # axis_label_col = c('black', 'black', 'black'),
  # axis_label_cex = c(1, 1, 1),
  #
  # axis_tick_x_at = 'x1',
  # axis_tick_lwd = c(1, 1),
  # axis_tick_length = c(-0.5, -0.5),

})
test_that('title options', {

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
