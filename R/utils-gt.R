#' @noRd
#' @autoglobal
gt_style <- function(gt_tbl,
                     lbl = TRUE,
                     tablign = "center",
                     tabsize = 16) {
  gt_tbl |>
    gt::fmt_markdown() |>
    gt::fmt_integer() |>
    gt::cols_align("left") |>
    gt::opt_table_font(
      font      = gt::google_font(name = "Atkinson Hyperlegible")) |>
    gt::tab_style(
      style     = gt::cell_text(
        align   = tablign,
        size    = gt::px(tabsize),
        font    = gt::google_font(name = "Fira Code"),
        weight  = "bold"),
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

#' @noRd
#' @autoglobal
gt_marks <- function(gt_tbl, cols) {

  gt_tbl |>
    gt::text_case_when(
      x == TRUE ~ gt::html(
        fontawesome::fa("check",
                        prefer_type = "solid",
                        fill = "red")),
      x == FALSE ~ gt::html(
        fontawesome::fa("xmark",
                        prefer_type = "solid",
                        fill = "white")),
      .default = NA,
      .locations = gt::cells_body(
        columns = {{ cols }}))
}
