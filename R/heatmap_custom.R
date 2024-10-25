#' カラーバーを描画する関数
#' @export
#'
#'

colorbar <- function(
    x, y,

    # colorbar detail
    axis_lim,
    axis_at,
    bar_col = cm.colors(7),
    n_bar_col = 30,
    horizontal = TRUE,
    axis_position = if (horizontal) 'top' else 'right',

    # size information
    bar_width = if (horizontal) 5 else 1,
    bar_height = if (horizontal) 1 else 5,
    margin = NULL, # bottom, left, top, right
    box_adj = c(0.5, 0.5),
    ticks_length = 0.2,

    # colors except for that inside colorbar
    bar_border_col = 'black',
    box_fill_col = NA,
    box_border_col = 'black',
    ticks_col = 'black',

    # line parameters (line type)
    bar_lty = 1,
    box_lty = 1,
    ticks_lty = 1,

    # line parameters (line width)
    bar_lwd = 1,
    box_lwd = 1,
    ticks_lwd = 1

) {

  # check arguments ============================================================
  # arguments with length 1
  arg_names <- c(
    'n_bar_col', 'horizontal', 'axis_position', 'bar_width', 'bar_height',
    'bar_border_col', 'box_fill_col', 'box_border_col',
    'bar_lty', 'box_lty', 'bar_lwd', 'box_lwd')
  for (arg_name in arg_names) {
    arg <- get(arg_name)
    if (length(arg) >= 2) {
      msg <- sprintf(
        'Multiple values were given as `%s`. Only the first value is used.',
        arg_name)
      warning(msg)
    }
    assign(arg_name, arg[1])
  }

  # axis_lim
  stopifnot(length(axis_lim) == 2)
  axis_lim <- as.numeric(axis_lim)

  # axis_at
  axis_at <- as.numeric(axis_at)

  # axis_position
  axis_positions <- c('none', 'top', 'bottom', 'right', 'left')
  sel_axis_position <- pmatch(axis_position, axis_positions)
  if (is.na(sel_axis_position)) stop('Invalid `axis_position`.')
  axis_position <- axis_positions[sel_axis_position]
  if (horizontal && axis_position %in% c('right', 'left')) {
    stop('`axis_position` should be `top` or `bottom` when `horizotal = TRUE`.')
  }
  if (!horizontal && axis_position %in% c('top', 'bottom')) {
    stop('`axis_position` should be `right` or `left` when `horizotal = FALSE`.')
  }

  # margin
  margin_names <- c('bottom', 'left', 'top', 'right')
  if (is.null(margin)) {
    margin <- c(1, 1, 1, 1)
    names(margin) <- margin_names
    margin[axis_position] <- margin[axis_position] + 1
    margin <- margin[margin_names]
  } else {
    names(margin) <- margin_names
  }



  # calculate coordinates ======================================================
  # convert arguments to coordinate scales in plot
  cx <- par('cxy')[1]
  cy <- par('cxy')[2] / par('cra')[2] * par('cra')[1]
  bar_width  <- bar_width  * cx
  bar_height <- bar_height * cy
  margin <- margin * c(cy, cx, cy, cx)
  ticks_length <- ticks_length * (if (horizontal) cy else cx)

  # box coordinates
  box_width  <- bar_width  + margin[2] + margin[4]
  box_height <- bar_height + margin[1] + margin[3]
  box_xleft   <- plot_coord_x(x) - box_width  * box_adj[1]
  box_xright  <- plot_coord_x(x) + box_width  * (1 - box_adj[1])
  box_ytop    <- plot_coord_y(y) + box_height * (1 - box_adj[2])
  box_ybottom <- plot_coord_y(y) - box_height * box_adj[2]

  # bar coordinates
  bar_xleft   <- box_xleft   + margin[2]
  bar_xright  <- box_xright  - margin[4]
  bar_ytop    <- box_ytop    - margin[3]
  bar_ybottom <- box_ybottom + margin[1]

  if (horizontal) {

    barseg_xall <- seq(bar_xleft, bar_xright, length.out = n_bar_col + 1)
    barseg_xleft   <- barseg_xall[-(n_bar_col + 1)]
    barseg_xright  <- barseg_xall[-1]
    barseg_ytop    <- bar_ytop
    barseg_ybottom <- bar_ybottom
    barseg_center  <- (barseg_xleft + barseg_xright) / 2

  } else {

    barseg_yall <- seq(bar_ytop, bar_ybottom, length.out = n_bar_col + 1)
    barseg_xleft   <- bar_xleft
    barseg_xright  <- bar_xright
    barseg_ytop    <- barseg_yall[-(n_bar_col + 1)]
    barseg_ybottom <- barseg_yall[-1]
    barseg_center  <- (barseg_ytop + barseg_ybottom) / 2

  }

  # axis coordinates
  axis_at_rel <- (axis_at - axis_lim[1]) / (axis_lim[2] - axis_lim[1])
  if (axis_position %in% c('top', 'bottom')) {
    axis_x0 <- barseg_center[1] * (1 - axis_at_rel) + barseg_center[n_bar_col] * axis_at_rel
    axis_x1 <- axis_x0
    if (axis_position == 'top') {
      axis_y0 <- bar_ytop
      axis_y1 <- axis_y0 + ticks_length
    } else {
      axis_y0 <- bar_ybottom
      axis_y1 <- axis_y0 - ticks_length
    }
  } else {
    axis_y0 <- barseg_center[1] * (1 - axis_at_rel) + barseg_center[n_bar_col] * axis_at_rel
    axis_y1 <- axis_y0
    if (axis_position == 'right') {
      axis_x0 <- bar_xright
      axis_x1 <- axis_x0 + ticks_length
    } else {
      axis_x0 <- bar_xleft
      axis_x1 <- axis_x0 - ticks_length
    }
  }


  # calculate graphical settings ===============================================
  # bar color
  bar_val <- seq(0, 1, length.out = n_bar_col)
  bar_fill_col <- rgb(colorRamp(bar_col)(bar_val) / 255)


  # plot =======================================================================
  old_xpd <- par('xpd')
  par(xpd = TRUE)

  # box
  rect(box_xleft, box_ybottom, box_xright, box_ytop,
       col = box_fill_col, border = box_border_col,
       lty = box_lty, lwd = box_lwd)

  # bar
  rect(barseg_xleft, barseg_ybottom, barseg_xright, barseg_ytop,
       col = bar_fill_col, border = NA)
  rect(bar_xleft, bar_ybottom, bar_xright, bar_ytop,
       col = NA, border = bar_border_col, lty = bar_lty, lwd = bar_lwd)

  # axis
  if (axis_position != 'none') {

    # ticks
    segments(axis_x0, axis_y0, axis_x1, axis_y1,
             col = ticks_col, lty = ticks_lty, lwd = ticks_lwd)

    # label
    # if (show_label) {
    #   y_label <- plot_coord_y(y + tick_len + space_height_label)
    #   text(x = x_at, y = y_label, labels = at,
    #        cex = cex_label, adj = c(0.5, 0))
    # }



  }

  par(xpd = old_xpd)

  # return =====================================================================
  ans <- list(
    box_xy = c(
      xleft  = box_xleft , ybottom = box_ybottom,
      xright = box_xright, ytop    = box_ytop))
  invisible(ans)

}

# colorbar <- function(
#     x, y, width, height, color, limits, at, n_color = 50, title = 'Value',
#     horizontal = TRUE, lwd_bar = 1, border_color = 'black',
#     tick_len = 0.02, lwd_ticks = 1,
#     show_label = TRUE, cex_label = 1, space_height_label = 0.02,
#     side_title = 'left',
#     space_width_title = 0.03, space_height_title = 0.03, cex_title = 1) {
#   # x <- 1.1
#   # y <- 1.08
#   # width <- 0.3
#   # height <- 0.03
#   # at <- c(1.3, 2, 3, 4)
#   # n_color <- 50
#
#   # カラーバー用の色を生成
#   val <- seq(0, 1, length.out = n_color)
#   color_all <- rgb(colorRamp(color)(val) / 255)
#
#   # 対応する座標を作成
#   x_all    <- plot_coord_x(seq(x, x + width, length.out = n_color + 1))
#   x_left   <- x_all[-(n_color + 1)]
#   x_right  <- x_all[-1]
#   x_center <- (x_left + x_right) / 2
#   y_top    <- plot_coord_y(y)
#   y_bottom <- plot_coord_y(y - height)
#
#   # カラーバーを描画
#   rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top,
#        col = color_all, border = NA)
#   rect(xleft  = x_left [1]      , ybottom = y_bottom,
#        xright = x_right[n_color], ytop    = y_top,
#        col = NA, border = 'black', lwd = lwd_bar)
#
#   # 目盛りを描画
#   at_relative <- (at - limits[1]) / (limits[2] - limits[1])
#   x_at <- x_center[1] * (1 - at_relative) + x_center[n_color] * at_relative
#   y_top_ticks <- plot_coord_y(y + tick_len)
#   if (tick_len > 0) {
#     segments(x0 = x_at, y0 = y_top, x1 = x_at, y1 = y_top_ticks,
#              lwd = lwd_ticks)
#   }
#
#   # ラベルを描画
#   if (show_label) {
#     y_label <- plot_coord_y(y + tick_len + space_height_label)
#     text(x = x_at, y = y_label, labels = at,
#          cex = cex_label, adj = c(0.5, 0))
#   }
#
#   # タイトルを描画
#   if (side_title == 'left') {
#     x_title <- plot_coord_x(x - space_width_title)
#     adj_title <- c(1, 0)
#   } else if (side_title == 'right') {
#     x_title <- plot_coord_x(x + width + space_width_title)
#     adj_title <- c(0, 0)
#   }
#   text(x = x_title,
#        y = plot_coord_y(y + space_height_title),
#        labels = title, cex = cex_title, adj = adj_title)
#
#   # パラメータを返す
#   x_df <- data.frame(left = x_left, center = x_center, right = x_right)
#   invisible(list(
#     x = x_df, y = c(y_top, y_bottom),
#     color = color_all, x_at = x_at))
#
# }
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

heatmap_custom <- function(
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
