#' カラーバーを描画する関数
#' @export
colorbar <- function(
    x, y, width, height, color, limits, at, n_color = 50, title = 'Value',
    horizontal = TRUE, lwd_bar = 1, border_color = 'black',
    tick_len = 0.02, lwd_ticks = 1,
    show_label = TRUE, cex_label = 1, space_height_label = 0.02,
    side_title = 'left',
    space_width_title = 0.03, space_height_title = 0.03, cex_title = 1) {
  # x <- 1.1
  # y <- 1.08
  # width <- 0.3
  # height <- 0.03
  # at <- c(1.3, 2, 3, 4)
  # n_color <- 50

  # カラーバー用の色を生成
  val <- seq(0, 1, length.out = n_color)
  color_all <- rgb(colorRamp(color)(val) / 255)

  # 対応する座標を作成
  x_all    <- plot_coord_x(seq(x, x + width, length.out = n_color + 1))
  x_left   <- x_all[-(n_color + 1)]
  x_right  <- x_all[-1]
  x_center <- (x_left + x_right) / 2
  y_top    <- plot_coord_y(y)
  y_bottom <- plot_coord_y(y - height)

  # カラーバーを描画
  rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top,
       col = color_all, border = NA)
  rect(xleft  = x_left [1]      , ybottom = y_bottom,
       xright = x_right[n_color], ytop    = y_top,
       col = NA, border = 'black', lwd = lwd_bar)

  # 目盛りを描画
  at_relative <- (at - limits[1]) / (limits[2] - limits[1])
  x_at <- x_center[1] * (1 - at_relative) + x_center[n_color] * at_relative
  y_top_ticks <- plot_coord_y(y + tick_len)
  if (tick_len > 0) {
    segments(x0 = x_at, y0 = y_top, x1 = x_at, y1 = y_top_ticks,
             lwd = lwd_ticks)
  }

  # ラベルを描画
  if (show_label) {
    y_label <- plot_coord_y(y + tick_len + space_height_label)
    text(x = x_at, y = y_label, labels = at,
         cex = cex_label, adj = c(0.5, 0))
  }

  # タイトルを描画
  if (side_title == 'left') {
    x_title <- plot_coord_x(x - space_width_title)
    adj_title <- c(1, 0)
  } else if (side_title == 'right') {
    x_title <- plot_coord_x(x + width + space_width_title)
    adj_title <- c(0, 0)
  }
  text(x = x_title,
       y = plot_coord_y(y + space_height_title),
       labels = title, cex = cex_title, adj = adj_title)

  # パラメータを返す
  x_df <- data.frame(left = x_left, center = x_center, right = x_right)
  invisible(list(
    x = x_df, y = c(y_top, y_bottom),
    color = color_all, x_at = x_at))

}
# plot(0, 0)
# cb <- colorbar(
#   x = 0.2, y = 0.6, width = 0.2, height = 0.05,
#   color = brewer.pal(9, 'RdBu'), limits = c(1.3, 4), at = c(1.3, 2, 3, 4),
#   title = expression(log[10](p)))

#' Heatmap on base plot
#'
#'
#'
#'
#'
#' @export

# x <- matrix(1:24, nrow = 6)
# colnames(x) <- LETTERS[1:4]
# rownames(x) <- letters[1:6]
# x
# row_group <- c('L1', '2', '1', '2', '1', '2')
# col_group <- c('1', '2', '2', '1')
# heatmap_on_baseplot(x, row_group, col_group)
# heatmap_on_baseplot(x, row_group, col_group, side_collab = 't')

# color = RColorBrewer::brewer.pal(11, 'RdBu')
# limit_color = NULL;
# cell_height = 1; cell_width = 1; sep_height = 0; sep_width = 0;
# groupsep_height = 0.5; groupsep_width = 0.5;
# cex_row = NULL; cex_col = NULL

heatmap_on_baseplot <- function(
    x, row_group = NULL, col_group = NULL, scale = 'none',
    color = RColorBrewer::brewer.pal(11, 'RdBu'),
    color_na = 'gray95',
    limits = NULL,
    cell_height = 1, cell_width = 1, sep_height = 0, sep_width = 0,
    groupsep_height = 0.7, groupsep_width = 0.7,
    side_rowlab = 'r', side_rowgrouplab = 'l',
    side_collab = 'b', side_colgrouplab = 't',
    space_rowlab = 0.01, space_rowgrouplab = 0.01,
    space_collab = 0.01, space_colgrouplab = 0.01,
    args_rowlab = NULL, args_rowgrouplab = NULL,
    args_collab = NULL, args_colgrouplab = NULL) {

  # parameter check
  # side_rowlab = c('right', 'left', 'none')
  # side_rowgrouplab = c('right', 'left', 'none')
  # side_collab = c('top', 'bottom', 'none')
  # side_colgrouplab = c('top', 'bottom', 'none')

  # side_rowlabのプログラムはシンプルにしたい -> axis_with_rot使えそう
  # side_collab, side_rowgrouplab, side_colgrouplab未実装
  # scaled未実装
  # カラーバー未実装

  # sort x
  orig_row <- rownames(x)
  orig_col <- colnames(x)
  order_row <- order(row_group)
  order_col <- order(col_group)
  x <- x[order_row, order_col]

  # sort row_group and col_group and convert them to factors
  row_group <- row_group[order_row]
  col_group <- col_group[order_col]
  if (!is.factor(row_group)) row_group <- factor(row_group)
  if (!is.factor(col_group)) col_group <- factor(col_group)

  # determine coordinates of cells
  cell_center_x <- seq_len(ncol(x)) * (cell_width + sep_width) +
    (as.integer(col_group) - 1) * groupsep_width
  cell_center_y <- rev(seq_len(nrow(x))) * (cell_height + sep_height) +
    (1 - as.integer(row_group)) * groupsep_height
  xleft   <- (cell_center_x - cell_width  / 2)[col(x)]
  xright  <- (cell_center_x + cell_width  / 2)[col(x)]
  ytop    <- (cell_center_y - cell_height / 2)[row(x)]
  ybottom <- (cell_center_y + cell_height / 2)[row(x)]

  # determine color
  if (is.null(limits)) limits <- range(x, na.rm = TRUE)
  x_scaled <- (x - limits[1]) / (limits[2] - limits[1])
  color_cell <- x_scaled
  color_cell[] <- color_na
  color_cell[!is.na(x_scaled)] <- rgb(colorRamp(color)(x_scaled[!is.na(x_scaled)]) / 255)

  # limits of x and y
  xlim <- range(cell_center_x) + cell_width  * c(-1, 1) / 2
  ylim <- range(cell_center_y) + cell_height * c(-1, 1) / 2

  # heatmap
  par(bty = 'n', xaxs = 'i', yaxs = 'i', xpd = TRUE)
  plot(NULL, xlim = xlim, ylim = ylim, type = 'n',
       xaxt = 'n', yaxt = 'n', ann = FALSE)
  rect(xleft, ybottom, xright, ytop,
       col = color_cell)

  # determine coordinates of row and column labels
  rowlab_y <- if (side_collab == 'b') {
    plot_coord_y(-space_rowlab)
  } else if (side_collab == 't') {
    plot_coord_y(1 + space_rowlab)
  }
  collab_x <- if (side_rowlab == 'r') {
    plot_coord_x(1 + space_collab)
  } else if (side_rowlab == 'l') {
    plot_coord_x(-space_collab)
  }

  # determine coordinates of group lables
  col_group_x <- tapply(cell_center_x, col_group, mean)
  col_group_y <- plot_coord_y(1 + space_colgrouplab)
  row_group_x <- plot_coord_x(-space_rowgrouplab)
  row_group_y <- tapply(cell_center_y, row_group, mean)

  # add labels of column
  if (side_collab == 'b') {
    if (!'srt' %in% names(args_collab)) {
      args_collab <- c(args_collab, list(adj = c(0, 0.5), srt = -90))
    }
    args_collab <- c(
      args_collab,
      list(x = cell_center_x, y = rowlab_y, labels = colnames(x)))
    do.call(text, args_collab)
  } else if (side_collab == 't') {
    if (!'srt' %in% names(args_collab)) {
      args_collab <- c(args_collab, list(adj = c(0, 0.5), srt = 90))
    }
    args_collab <- c(
      args_collab,
      list(x = cell_center_x, y = rowlab_y, labels = colnames(x)))
    do.call(text, args_collab)
  }

  # add labels of col_group
  if (side_colgrouplab != 'n') {
    if (!'srt' %in% names(args_colgrouplab)) {
      args_colgrouplab <- c(args_colgrouplab, list(adj = c(0.5, 0), srt = 0))
    }
    args_colgrouplab <- c(
      args_colgrouplab,
      list(x = col_group_x, y = col_group_y, labels = levels(col_group)))
    do.call(text, args_colgrouplab)
  }

  # add labels of row
  if (side_rowlab != 'n') {
    if (!'srt' %in% names(args_rowlab)) {
      args_rowlab <- c(args_rowlab, list(adj = c(0, 0.5), srt = 0))
    }
    args_rowlab <- c(
      args_rowlab,
      list(x = collab_x, y = cell_center_y, labels = rownames(x)))
    do.call(text, args_rowlab)
  }

  # add labels of row_group
  if (side_rowgrouplab != 'n') {
    if (!'srt' %in% names(args_rowgrouplab)) {
      args_rowgrouplab <- c(args_rowgrouplab, list(adj = c(0.5, 0), srt = 90))
    }
    args_rowgrouplab <- c(
      args_rowgrouplab,
      list(x = row_group_x, y = row_group_y, labels = levels(row_group)))
    do.call(text, args_rowgrouplab)
  }

  invisible(list(
    cell_height = cell_height, cell_width = cell_width,
    sep_height = sep_height, sep_width = sep_width,
    groupsep_height = groupsep_height, groupsep_width = groupsep_width,
    cell_center_x = cell_center_x, cell_center_y = cell_center_y,
    xleft = xleft, xright = xright, ytop = ytop, ybottom = ybottom,
    xlim = xlim, ylim = ylim, rowlab_y = rowlab_y, collab_x = collab_x,
    order_row = order_row, order_col = order_col,
    col_group_x = col_group_x, col_group_y = col_group_y,
    row_group_x = row_group_x, row_group_y = row_group_y))

}
