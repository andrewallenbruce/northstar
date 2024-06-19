#' {gt} Theme
#'
#' @param gt_object `<gt_tbl>` A [gt][gt::gt-package] table object
#'
#' @param column_labels `<lgl>` Show column labels, default is `TRUE`
#'
#' @param tab_align `<chr>` Stub text alignment, default is `center`
#'
#' @param tab_size `<int>` Font size, default is `16`
#'
#' @param tab_weight description
#'
#' @param ... Optional additional arguments to [gt::table_options()][gt::gt-package]
#'
#' @returns An object of class `<gt_tbl>`
#'
#' @autoglobal
#'
#' @export
gt_theme_northstar <- function(gt_object,
                               column_labels = TRUE,
                               tab_align = "center",
                               tab_size = 16,
                               tab_weight = "normal",
                               ...) {

  stopifnot(
    "`gt_object` must be a `gt_tbl`" = "gt_tbl" %in% class(
      gt_object
      )
    )

  gt_object |>
    gt::cols_align("left") |>
    gt::opt_table_font(
      font = gt::google_font(
        name = "Atkinson Hyperlegible")) |>
    gt::tab_style(
      style = gt::cell_text(
        align   = tab_align,
        size    = gt::px(tab_size),
        font    = gt::google_font(name = "Fira Code"),
        weight  = tab_weight),
      locations = gt::cells_stub()) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "powderblue"),
        gt::cell_text(weight = "bold"),
        gt::cell_borders(
          sides = c("all"),
          color = "powderblue",
          weight = gt::px(3)
      )
    ),
    locations = gt::cells_row_groups()) |>
    gt::tab_options(
      column_labels.hidden = column_labels,
      column_labels.text_transform = if (column_labels) "uppercase" else NULL,
      heading.align = "left",
      heading.title.font.size = gt::px(16),
      heading.subtitle.font.size = gt::px(16),
      quarto.disable_processing = TRUE,
      row_group.as_column = TRUE,
      row_group.font.size = gt::px(24),
      source_notes.font.size = gt::px(16),
      table.font.size = gt::px(16),
      table.width = gt::pct(100),
      ...
    )
}
