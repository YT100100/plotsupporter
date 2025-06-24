test_that('standard', {

  dat <- expand.grid(x1 = factor(letters[1:5]),
                     x2 = factor(LETTERS[1:3]),
                     rep = 1:10)
  set.seed(1)
  dat <- within(dat, {
    y <- as.integer(x1) + as.integer(x2) * 3 + rnorm(nrow(dat), sd = 2)
  })
  # debugonce(easy_barplot)
  easy_barplot(x1 = dat$x1, y = dat$y)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y)
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'tukey')
  easy_barplot(x1 = dat$x1, x2 = dat$x2, y = dat$y, test_method = 'dunnett')


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
