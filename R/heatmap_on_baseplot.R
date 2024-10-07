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
# row_group <- c('1', '2', '1', '2', '1', '2')
# col_group <- c('1', '2', '2', '1')
# heatmap_on_baseplot(x, row_group, col_group)

heatmap_on_baseplot <- function(
    x, row_group = NULL, col_group = NULL, scale = 'none',
    color = RColorBrewer::brewer.pal(11, 'RdBu'),
    limit_color = NULL,
    cell_height = 1, cell_width = 1, sep_height = 0, sep_width = 0,
    groupsep_height = 0.5, groupsep_width = 0.5,
    cex_row = NULL, cex_col = NULL) {

  # scaled未実装

  # sort x
  orig_row <- rownames(x)
  orig_col <- colnames(x)
  x <- x[order(row_group), order(col_group)]

  # sort row_group and col_group and convert them to factors
  row_group <- row_group[match(rownames(x), orig_row)]
  col_group <- col_group[match(colnames(x), orig_col)]
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
  if (is.null(limit_color)) limit_color <- range(x, na.rm = TRUE)
  x_scaled <- (x - limit_color[1]) / (limit_color[2] - limit_color[1])
  color_cell <- rgb(colorRamp(color)(x_scaled) / 255)

  # limits of x and y
  xlim <- range(cell_center_x) + cell_width  * c(-1, 1) / 2
  ylim <- range(cell_center_y) + cell_height * c(-1, 1) / 2

  # heatmap
  op <- par(no.readonly = TRUE)
  par(bty = 'n', xaxs = 'i', yaxs = 'i', xpd = TRUE)
  plot(NULL, xlim = xlim, ylim = ylim, type = 'n',
       xaxt = 'n', yaxt = 'n',)
  rect(xleft, ybottom, xright, ytop,
       col = color_cell)
  text(x = par()$usr[1], y = cell_center_y, labels = rownames(x),
       adj = c(1, 0.5))
  text(x = cell_center_x, y = par()$usr[3], labels = colnames(x),
       adj = c(1, 0.5), srt = 90)
  par(op)

}
