create_label <- function(aov_res, test_method) {

  if (is.null(test_method)) {

    return(NA)

  } else if (identical(test_method, 'tukey')) {

    glht_res_i <- glht(aov_res, linfct = mcp(x1 = 'Tukey'))
    return(cld(glht_res_i, decreasing = FALSE)$mcletters$Letters)

  } else if (identical(test_method, 'dunnett')) {

    glht_res <- glht(aov_res, linfct = mcp(x1 = 'Dunnett'))
    pval <- summary(glht_res)$test$pvalues
    mark <- convert_pval_to_txt(pval)
    return(c('', mark))

  } else {

    stop('Wrong `test_method.`')

  }

}

#' Easy making of barplot
#'
#' This function is a wrapper of \code{\link[graphics]{barplot}} function.
#'
#' @param y A numeric vector.
#' @param x1 A factor vector used for grouping \code{y}.
#'   The length should be the same as \code{y}.
#' @param x2 An optional factor vector used for grouping bars.
#'   The length should be the same as \code{y}.
#'
#' @param test_method A character determining how the differences among groups
#'   should be tested. \code{'tukey'} or \code{'dunnett'} can be given.
#'   If \code{NULL}, no test will be applied.
#'   If \code{x2} is provided, the tests will be applied for
#'   each groups of \code{x2} independently.
#' @param signif_label_at_top Logical.
#'   Should the labels shown at the top of the bars?
#' @param signif_label_size Label size.
#' @param signif_label_space Space between labels and any nearest object
#'   (either of bars, error bars, or points).
#'
#' @param bar_space_x1 Space width between bars.
#'   Default value is 0.2 if \code{x2} is \code{NULL} and otherwise 0.
#' @param bar_space_x2 Space width between bar groups. Default value is 1.
#' @param bar_col Color inside bars.
#' @param bar_border Color of bar borders.
#' @param bar_lwd Line width of bar borders.
#'
#' @param show_errorbar Logical. Should error bars be shown?
#' @param errorbar_shift How much x coordinates of the error bars
#'   should be shifted from the center of the bars.
#'   It will be set at \code{-0.2} if \code{show_point} is \code{TRUE},
#'   otherwise \code{0}.
#' @param errorbar_length Length of horizontal lines of error bars.
#' @param errorbar_col Color of error bars.
#' @param errorbar_lwd Line width of error bars.
#'
#' @param show_point Logical. Should the data points be shown?
#' @param point_shift How much x coordinates of the points
#'   should be shifted from the center of the bars.
#'   It will be set at \code{0.2} if \code{show_errorbar} is \code{TRUE},
#'   otherwise \code{0}.
#' @param point_color Point color. If a vector is given, color
#' @param point_bg Background color of points
#'   (applied if \code{point_pch} is in 21-25).
#' @param point_cex Point size.
#' @param point_pch Point shape.
#' @param point_lwd Point line width.
#'
#' @param axis_label_x2_line
#'   Distance between panel border and x-axis label of \code{x2}.
#'
#' @param y_space Space between the highest/lowest object and panel border.
#'
#' @param main Title.
#' @param xlab Title of x-axis.
#' @param ylab Title of y-axis.
#' @param main_line Distance between panel border and title.
#' @param main_cex Title size.
#' @param main_font Title font.
#' @param main_col Title color.
#' @param xlab_line Distance between panel border and title of x-axis.
#' @param xlab_cex Title size of x-axis.
#' @param xlab_font Title font of x-axis.
#' @param xlab_col Title size of x-axis.
#' @param ylab_line Distance between panel border and title of y-axis.
#' @param ylab_cex Title size of y-axis.
#' @param ylab_font Title font of y-axis.
#' @param ylab_col Title size of y-axis.
#'
#' @seealso \code{\link[graphics]{barplot}}
#'
#' @export

easy_barplot <- function(
    y, x1, x2 = NULL,

    test_method = NULL,
    signif_label_at_top = TRUE,
    signif_label_cex = 1,
    signif_label_space = 0.03,
    # 拡充可能

    bar_space_x1 = NA,
    bar_space_x2 = NA,
    bar_col = NULL,
    bar_border = 'black',
    bar_lwd = 1,
    # 拡充可能

    show_errorbar = TRUE,
    errorbar_shift = NA,
    errorbar_length = 0.05,
    errorbar_col = 'black',
    errorbar_lwd = 1,

    show_point = TRUE,
    point_shift = NA,
    point_color = 'gray20',
    point_bg = 'white',
    point_cex = 1,
    point_pch = 1,
    point_lwd = 1,

    axis_label_x2_line = 2,
    # 拡充可能

    # legend関連拡充可能

    y_space = 0.03,

    main = '',
    xlab = '',
    ylab = '',

    main_line = NA,
    main_cex = 1.3,
    main_font = 2,
    main_col = 'black',
    xlab_line = NA,
    xlab_cex = 1,
    xlab_font = 1,
    xlab_col = 'black',
    ylab_line = NA,
    ylab_cex = 1,
    ylab_font = 1,
    ylab_col = 'black') {


  # 引数を適正に処理 ===========================================================
  # x2が与えられない場合、ダミーの変数を作成する
  does_x2_exist <- !is.null(x2)
  if (!does_x2_exist) x2 <- rep('A', length(y))

  # x1, x2を因子に変換
  # glht関数はxがfactorでないとエラーを出す
  x1 <- as.factor(x1)
  x2 <- as.factor(x2)

  # 棒の間のスペースを設定
  if (is.na(bar_space_x1)) {
    bar_space_x1 <- if (does_x2_exist) 0 else 0.2
  }
  if (is.na(bar_space_x2)) {
    bar_space_x2 <- 1
  }

  # 点を表示するか否かでエラーバーの表示位置を調整
  if (is.na(errorbar_shift)) {
    if (show_point) {
      errorbar_shift <- -0.1
    } else {
      errorbar_shift <- 0
    }
  }

  # エラーバーを表示するか否かでデータ点の表示場所を調整
  if (is.na(point_shift)) {
    if (show_errorbar) {
      point_shift <- 0.2
    } else {
      point_shift <- 0
    }
  }


  # 必要な数値を算出 ===========================================================
  # 平均と標準偏差を計算
  mat_mean <- tapply(y, list(x1, x2), mean, na.rm = TRUE)
  mat_sd   <- tapply(y, list(x1, x2), sd  , na.rm = TRUE)

  # 差を検定
  label <- matrix(NA, nrow = nlevels(x1), ncol = nlevels(x2))
  rownames(label) <- levels(x1)
  colnames(label) <- levels(x2)
  for (x2_lev_i in levels(x2)) {

    aov_dat_i <- data.frame(x1 = x1, x2 = x2, y = y)
    aov_dat_i <- subset(aov_dat_i, x2 == x2_lev_i)
    aov_res_i <- aov(y ~ x1, data = aov_dat_i)
    label[, x2_lev_i] <- create_label(aov_res_i, test_method)

  }


  # 作図の詳細を決定 ===========================================================
  # エラーバーのy座標
  errorbar_ymin <- mat_mean - mat_sd
  errorbar_ymax <- mat_mean + mat_sd

  # y軸の範囲を決める
  ylim <- range(mat_mean, na.rm = TRUE)
  ylim[1] <- pmin(ylim[1], 0)
  ylim[2] <- pmax(ylim[2], 0)
  if (show_errorbar) {
    ylim <- range(c(ylim, errorbar_ymin, errorbar_ymax), na.rm = TRUE)
  }
  if (show_point) {
    ylim <- range(c(ylim, y), na.rm = TRUE)
  }

  # 有意ラベルのy座標
  if (signif_label_at_top) {

    point_ymax <- tapply(y, list(x1, x2), max, na.rm = TRUE)
    if (show_errorbar & show_point) {
      signif_label_y <- pmax(errorbar_ymax, point_ymax)
    } else if (show_errorbar) {
      signif_label_y <- errorbar_ymax
    } else if (show_point) {
      signif_label_y <- point_ymax
    } else {
      signif_label_y <- mat_mean
    }
    signif_label_y <- signif_label_y + diff(ylim) * signif_label_space

  } else {

    point_ymin <- tapply(y, list(x1, x2), min, na.rm = TRUE)
    if (show_errorbar & show_point) {
      signif_label_y <- pmax(errorbar_ymin, point_ymin)
    } else if (show_errorbar) {
      signif_label_y <- errorbar_ymin
    } else if (show_point) {
      signif_label_y <- point_ymin
    } else {
      signif_label_y <- mat_mean
    }
    signif_label_y <- signif_label_y - diff(ylim) * signif_label_space

  }

  # y軸の範囲を更新
  ylim <- range(c(ylim, signif_label_y), na.rm = TRUE)
  if (ylim[1] != 0) ylim[1] <- ylim[1] - diff(ylim) * y_space
  if (ylim[2] != 0) ylim[2] <- ylim[2] + diff(ylim) * y_space


  # 作図 =======================================================================
  # 棒グラフ
  op <- par(lwd = bar_lwd)
  bar_pos <- barplot(
    mat_mean, beside = TRUE, ylim = ylim, xaxt = 'n',
    space = c(bar_space_x1, bar_space_x2),
    col = bar_col, border = bar_border)
  par(op)
  rownames(bar_pos) <- levels(x1)
  colnames(bar_pos) <- levels(x2)

  # ラベル
  text(x = bar_pos, y = signif_label_y,
       labels = label, cex = signif_label_cex)

  # エラーバー
  if (show_errorbar) {
    arrows(x0 = bar_pos + errorbar_shift, y0 = errorbar_ymin,
           x1 = bar_pos + errorbar_shift, y1 = errorbar_ymax,
           length = errorbar_length, angle = 90, code = 3,
           col = errorbar_col, lwd = errorbar_lwd)
  }

  # データ点
  if (show_point) {
    points(x = bar_pos[cbind(x1, x2)] + point_shift, y = y,
           pch = point_pch, col = point_color, bg = point_bg, cex = point_cex,
           lwd = point_lwd)
  }

  # 軸
  axis(1, at = bar_pos, labels = NA)
  axis_label(1, at = bar_pos, labels = rep(rownames(bar_pos), ncol(bar_pos)),
             srt = 90)
  axis_label(1, at = colMeans(bar_pos), labels = levels(x2),
             line = axis_label_x2_line)

  # タイトル・軸タイトル
  title(main = main, line = main_line,
        cex.main = main_cex, font.main = main_font, col.main = main_col)
  title(xlab = xlab, line = xlab_line,
        cex.lab = xlab_cex, font.lab = xlab_font, col.lab = xlab_col)
  title(ylab = ylab, line = ylab_line,
        cex.lab = ylab_cex, font.lab = ylab_font, col.lab = ylab_col)

  # パネル
  box()

}

