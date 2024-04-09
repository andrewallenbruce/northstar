#' gt Theme
#' @param gt_tbl description
#' @param lbl description
#' @param tablign description
#' @param tabsize description
#' @param tabwt description
#' @return description
#' @export
#' @keywords internal
#' @autoglobal
gt_style <- function(gt_tbl,
                     lbl = TRUE,
                     tablign = "center",
                     tabsize = 16,
                     tabwt = "normal") {
  gt_tbl |>
    # gt::fmt_markdown() |>
    gt::cols_align("left") |>
    gt::opt_table_font(
      font      = gt::google_font(name = "Atkinson Hyperlegible")) |>
    gt::tab_style(
      style     = gt::cell_text(
        align   = tablign,
        size    = gt::px(tabsize),
        font    = gt::google_font(name = "Fira Code"),
        weight  = tabwt),
      locations = gt::cells_stub()) |>
    gt::tab_options(
      column_labels.hidden       = lbl,
      table.font.size            = gt::px(16),
      table.width                = gt::pct(100),
      heading.align              = "left",
      heading.title.font.size    = gt::px(16),
      heading.subtitle.font.size = gt::px(16),
      source_notes.font.size     = gt::px(16),
      row_group.as_column        = TRUE,
      row_group.font.size        = gt::px(24)
    )
}
