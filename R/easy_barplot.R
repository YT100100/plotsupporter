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

    mark <- c('', mark)
    names(mark) <- aov_res$xlevels$x1
    return(mark)

  } else {

    stop('Wrong `test_method.`')

  }

}

extend_ylim <- function(ylim, y, panel_space_prop) {

  ylim_wo_space <- ylim + c(1, -1) * diff(ylim) * panel_space_prop
  y_min <- min(y, na.rm = TRUE)
  y_max <- max(y, na.rm = TRUE)

  if (y_min < ylim_wo_space[1] && y_max > ylim_wo_space[2]) {

    ylim[1] <- y_min - (y_max - y_min) * panel_space_prop
    ylim[2] <- y_max + (y_max - y_min) * panel_space_prop

  } else if (y_min < ylim_wo_space[1]) {

    ylim[1] <- y_min - (ylim[2] - y_min) * panel_space_prop

  } else if (y_max > ylim_wo_space[2]) {

    ylim[2] <- y_max + (y_max - ylim[1]) * panel_space_prop

  }
  ylim

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
#' @param remove_level_without_data Logical. Should the levels of \code{x1}
#'   without data be removed?
#'
#' @param test_method A character determining how the differences among groups
#'   should be tested. \code{'tukey'} or \code{'dunnett'} can be given.
#'   If \code{NULL}, no test will be applied.
#'   If \code{x2} is provided, the tests will be applied for
#'   each groups of \code{x2} independently.
#' @param show_ns Logical. Should the text "n.s." be shown
#'   if the difference was not significantly different from 0?
#' @param signif_label_at_top Logical.
#'   Should the labels shown at the top of the bars?
#' @param signif_label_size Label size.
#' @param signif_label_space Space between labels and any nearest object
#'   (either of bars, error bars, or points).
#' @param signif_label_adj Adjustment of the labels.
#'   A numeric vector of length two of which elements in \eqn{[0, 1]}
#'   should be given.
#'   The label position is adjusted to left if the first element is 0
#'   and right if 1.
#'   The label position is adjusted to bottom if the second element is 0
#'   and top if 1.
#' @param signif_label_col Label color.
#' @param signif_label_font Label font.
#' @param signif_label_srt Label rotation angle.
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
#' @param show_axis_label Logical vector with three elements
#'   indicating whether the \code{x1}, \code{x2}, and \code{y} axis labels
#'   should be written.
#' @param axis_label_x1 Axis labels for \code{x1}.
#'   Its length must be the same as the number of levels of \code{x1}.
#'   If \code{NULL}, levels of \code{x1} is given.
#' @param axis_label_x2 Axis labels for \code{x2}.
#'   Its length must be the same as the number of levels of \code{x2}.
#'   If \code{NULL}, levels of \code{x2} is given.
#' @param axis_label_line Numeric vector with three elements
#'   indicating the distance between axis labels and panel
#'   for \code{x1}, \code{x2}, and \code{y}.
#' @param axis_label_srt Numeric vector with three elements
#'   indicating the angle of axis labels
#'   for \code{x1}, \code{x2}, and \code{y}.
#' @param axis_label_font Numeric vector with three elements
#'   indicating the font of axis labels
#'   for \code{x1}, \code{x2}, and \code{y}.
#' @param axis_label_col Numeric vector with three elements
#'   indicating color of axis labels
#'   for \code{x1}, \code{x2}, and \code{y}.
#' @param axis_label_cex Numeric vector with three elements
#'   indicating the size of axis labels
#'   for \code{x1}, \code{x2}, and \code{y}.
#'
#' @param axis_tick_x_at If \code{'x1'} or \code{'x2'},
#'   tick marks of x-axis will be drawn
#'   at the place of axis labels of \code{x1} or \code{x2}, respectively.
#'   Otherwise, the tick marks will not be drawn.
#' @param axis_tick_lwd Line widths of the tick marks in x- and y- axes.
#' @param axis_tick_length Lengths of the tick marks in x- and y- axes.
#'   Negative values indicate that the tick marks will be drawn toward
#'   outside of the panel. If positive, it will be toward inside of the panel.
#'
#' @param axis_label_x2_line
#'   Distance between panel border and x-axis label of \code{x2}.
#'
#' @param panel_space_prop Space between the highest/lowest object and panel border.
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

    remove_level_without_data = FALSE,

    test_method = NULL,
    # test_target = c('x1', 'x2', 'x1x2'),
    # compare_with_control = FALSE,
    # control_level = NULL,
    # assume_normality = TRUE,

    show_ns = TRUE,
    signif_label_at_top = TRUE,
    signif_label_cex = 1,
    signif_label_space = 0.03,
    signif_label_adj = c(0.5, 0.5),
    signif_label_col = 'black',
    signif_label_font = 1,
    signif_label_srt = 0,

    bar_space_x1 = NA,
    bar_space_x2 = NA,
    bar_col = NULL,
    bar_border = 'black',
    bar_lwd = 1,

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

    show_axis_label = c(TRUE, TRUE, TRUE),
    axis_label_x1 = NULL,
    axis_label_x2 = NULL,
    axis_label_line = c(1, 2, 1),
    axis_label_srt = c(90, 0, 90),
    axis_label_font = c(1, 1, 1),
    axis_label_col = c('black', 'black', 'black'),
    axis_label_cex = c(1, 1, 1),

    axis_tick_x_at = 'x1',
    axis_tick_lwd = c(1, 1),
    axis_tick_length = c(-0.5, -0.5),


    # legend関連拡充可能

    panel_space_prop = 0.03,

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

  # x1とx2の組み合わせを作成
  x1x2_levs <- expand.grid(x1 = levels(x1), x2 = levels(x2))
  x1x2_levs$n <- sapply(seq_len(nrow(x1x2_levs)), function(i) {
    vals <- y[x1 == x1x2_levs[i, 'x1'] & x2 == x1x2_levs[i, 'x2']]
    vals <- vals[!is.na(vals)]
    length(vals)
  })
  if (remove_level_without_data) {
    x1x2_levs <- subset(x1x2_levs, n >= 1)
  }

  # 棒のスペースを作成
  x2_len <- cumsum(rle(as.character(x1x2_levs$x2))$lengths)
  x1x2_levs$space <- bar_space_x1
  x1x2_levs$space[x2_len[-length(x2_len)] + 1] <- bar_space_x2

  # 平均と標準偏差を計算
  x1x2_levs$meanval <- sapply(seq_len(nrow(x1x2_levs)), function(i) {
    vals <- y[x1 == x1x2_levs[i, 'x1'] & x2 == x1x2_levs[i, 'x2']]
    mean(vals, na.rm = TRUE)
  })
  x1x2_levs$sdval <- sapply(seq_len(nrow(x1x2_levs)), function(i) {
    vals <- y[x1 == x1x2_levs[i, 'x1'] & x2 == x1x2_levs[i, 'x2']]
    sd(vals, na.rm = TRUE)
  })

  # 差を検定
  x1x2_levs$label <- NA
  for (x2_lev_i in levels(x2)) {

    # 分散分析
    aov_dat_i <- data.frame(x1 = x1, x2 = x2, y = y)
    aov_dat_i <- subset(aov_dat_i, x2 == x2_lev_i)
    aov_res_i <- aov(y ~ x1, data = aov_dat_i)

    # 検定
    label_i <- create_label(aov_res_i, test_method)

    # 検定結果を保存
    sel_i <- x1x2_levs$x2 == x2_lev_i
    matcher_i <- match(names(label_i), x1x2_levs[sel_i, 'x1'])
    x1x2_levs[sel_i, 'label'][matcher_i] <- label_i

  }

  # "n.s."を表示しない場合は消す
  x1x2_levs$label <- gsub('^n\\.s\\.$', '', x1x2_levs$label)


  # 作図の詳細を決定 ===========================================================
  # エラーバーのy座標
  x1x2_levs <- within(x1x2_levs, {
    errorbar_ymin <- meanval - sdval
    errorbar_ymax <- meanval + sdval
  })

  # y軸の範囲を決める
  ylim <- range(x1x2_levs$meanval, na.rm = TRUE)
  ylim[1] <- pmin(ylim[1], 0)
  ylim[2] <- pmax(ylim[2], 0)
  if (show_errorbar) {
    ylim <- extend_ylim(
      ylim, x1x2_levs[, c('errorbar_ymin', 'errorbar_ymax')], panel_space_prop)
  }
  if (show_point) {
    ylim <- extend_ylim(ylim, y, panel_space_prop)
  }

  # 有意ラベルのy座標
  if (signif_label_at_top) {

    # 各xの点の中で最大の値を取得
    x1x2_levs$point_ymax <- sapply(seq_len(nrow(x1x2_levs)), function(i) {
      vals <- y[x1 == x1x2_levs[i, 'x1'] & x2 == x1x2_levs[i, 'x2']]
      if (length(vals) == 0) return(NA)
      max(vals, na.rm = TRUE)
    })

    # 表示モードに従って座標を決定
    x1x2_levs$signif_label_y <- if (show_errorbar & show_point) {
      pmax(x1x2_levs$errorbar_ymax, x1x2_levs$point_ymax)
    } else if (show_errorbar) {
      x1x2_levs$errorbar_ymax
    } else if (show_point) {
      x1x2_levs$point_ymax
    } else {
      x1x2_levs$meanval
    }

    # 他オブジェクトとの距離を確保
    x1x2_levs$signif_label_y <-
      x1x2_levs$signif_label_y + diff(ylim) * signif_label_space

  } else {

    # 各xの点の中で最大の値を取得
    x1x2_levs$point_ymin <- sapply(seq_len(nrow(x1x2_levs)), function(i) {
      vals <- y[x1 == x1x2_levs[i, 'x1'] & x2 == x1x2_levs[i, 'x2']]
      min(vals, na.rm = TRUE)
    })

    # 表示モードに従って座標を決定
    x1x2_levs$signif_label_y <- if (show_errorbar & show_point) {
      pmin(x1x2_levs$errorbar_ymin, x1x2_levs$point_ymin)
    } else if (show_errorbar) {
      x1x2_levs$errorbar_ymin
    } else if (show_point) {
      x1x2_levs$point_ymin
    } else {
      x1x2_levs$meanval
    }

    # 他オブジェクトとの距離を確保
    x1x2_levs$signif_label_y <-
      x1x2_levs$signif_label_y - diff(ylim) * signif_label_space

  }

  # y軸の範囲を更新
  if (!is.null(test_method)) {
    ylim <- extend_ylim(ylim, x1x2_levs$signif_label_y, panel_space_prop)
  }


  # 作図 =======================================================================
  # 棒グラフ
  op <- par(lwd = bar_lwd)
  x1x2_levs$bar_pos <- barplot(
    x1x2_levs$meanval, beside = TRUE, space = x1x2_levs$space,
    ylim = ylim, xaxt = 'n', yaxt = 'n',
    col = bar_col, border = bar_border)
  par(op)

  # ラベル
  with(x1x2_levs, {
    text(x = bar_pos, y = signif_label_y, labels = label,
         cex = signif_label_cex, adj = signif_label_adj,
         col = signif_label_col, font = signif_label_font,
         srt = signif_label_srt)
  })

  # エラーバー
  if (show_errorbar) {
    with(x1x2_levs, {
      arrows(x0 = bar_pos + errorbar_shift, y0 = errorbar_ymin,
             x1 = bar_pos + errorbar_shift, y1 = errorbar_ymax,
             length = errorbar_length, angle = 90, code = 3,
             col = errorbar_col, lwd = errorbar_lwd)
    })
  }

  # データ点
  if (show_point) {
    point_x <- mapply(function(x1_i, x2_i) {
      x1x2_levs$bar_pos[x1x2_levs$x1 == x1_i & x1x2_levs$x2 == x2_i]
    }, x1_i = x1, x2_i = x2)
    with(x1x2_levs, {
      points(x = point_x + point_shift, y = y,
             pch = point_pch, col = point_color, bg = point_bg, cex = point_cex,
             lwd = point_lwd)
    })
  }

  # x軸目盛
  bar_pos_x2mean <- tapply(x1x2_levs$bar_pos, x1x2_levs$x2, mean)
  axis_x_at <- if (identical(axis_tick_x_at, 'x1')) {
    x1x2_levs$bar_pos
  } else if (identical(axis_tick_x_at, 'x2')) {
    bar_pos_x2mean
  } else {
    NULL
  }
  axis(1, at = axis_x_at, labels = NA,
       lwd.ticks = axis_tick_lwd[1], tcl = axis_tick_length[1])

  # y軸目盛
  axis_y_at <- axis(
    2, labels = NA,
    lwd.ticks = axis_tick_lwd[2], tcl = axis_tick_length[2])

  # 軸ラベル
  if (show_axis_label[1]) {
    axis_label(
      1, at = x1x2_levs$bar_pos, labels = x1x2_levs$x1,
      line = axis_label_line[1], srt = axis_label_srt[1],
      font = axis_label_font[1], col = axis_label_col[1],
      cex  = axis_label_cex [1])
  }
  if (show_axis_label[2]) {
    axis_label(
      1, at = bar_pos_x2mean, labels = names(bar_pos_x2mean),
      line = axis_label_line[2], srt = axis_label_srt[2],
      font = axis_label_font[2], col = axis_label_col[2],
      cex  = axis_label_cex [2])
  }
  if (show_axis_label[3]) {
    axis_label(
      2, at = axis_y_at,
      line = axis_label_line[3], srt = axis_label_srt[3],
      font = axis_label_font[3], col = axis_label_col[3],
      cex  = axis_label_cex [3])
  }

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

