#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @noRd
mount_board <- function(source = c("local", "remote")) {

  source <- match.arg(source)

  switch(
    source,
    local = pins::board_folder(fs::path_package("extdata/pins", package = "northstar")),
    remote = pins::board_url("https://raw.githubusercontent.com/andrewallenbruce/northstar/master/inst/extdata/pins/"))
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @template args-dots
#'
#' @returns `<tibble>`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match0(pin, list_pins())

  pins::pin_read(board, pin)

}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<list>` of [pins][pins::pins-package]
#'
#' @autoglobal
#'
#' @noRd
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)

}

#' Load Example Datasets
#'
#' @param name name of example
#'
#' @returns `<tibble>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
get_example <- function(name = c("report", "practicum")) {

  name <- match.arg(name)

  get_pin("examples")[[name]] |> .add_class()

}

#' Alias for `as.character()`
#'
#' @param ... arguments to pass to `as.character()`
#'
#' @keywords internal
#'
#' @export
chr <- function(...) {
  as.character(...)
}

#' Apply {gt} Theme
#'
#' @param gt_object `<gt_tbl>` A [gt][gt::gt-package] table object
#'
#' @param column_labels `<lgl>` Show column labels, default is `TRUE`
#'
#' @param tab_align `<chr>` Stub text alignment, default is `center`
#'
#' @param tab_size `<int>` Font size, default is `16`
#'
#' @param tab_weight Font weight, default is `bold`
#'
#' @param ... Additional arguments to [gt::table_options()][gt::gt-package]
#'
#' @returns An object of class `<gt_tbl>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
gt_theme_northstar <- function(gt_object,
                               no_column_labels = TRUE,
                               tab_align = "center",
                               tab_size = 16,
                               tab_weight = "bold",
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
          weight = gt::px(2)
        )
      ),
      locations = gt::cells_row_groups()) |>
    gt::tab_options(
      column_labels.hidden = no_column_labels,
      column_labels.text_transform = if (!no_column_labels) "capitalize" else NULL,
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

#' Data dictionary for PFS and RVU data
#'
#' @param out description
#'
#' @param dict description
#'
#' @returns description
#'
#' @examples
#' data_dict()
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
data_dict <- function(out = c("md", "df"), dict = c("pfs", "rvu")) {

  out  <- match.arg(out)
  dict <- match.arg(dict)
  res  <- switch(dict, pfs = dict_pfs(), rvu = dict_rvu())

  switch(
    out,
    df = res,
    md = res |>
      dplyr::mutate(
        var = glue::glue("`{var}`"),
        label = glue::glue("**{label}**")
        )
    )
}

#' @autoglobal
#'
#' @noRd
dict_pfs <- function() {

  dplyr::tribble(
    ~var,        ~label,                              ~description,
    #----        #-----                              #-----------
    "mac",       "Carrier Number",                   "Medicare Administrative Contractor ID",
    "locality",  "Locality",                         "Pricing Locality ID",
    "hcpcs",     "HCPCS Code",                       "HCPCS Code",
    "mod",       "Modifier",                         "Diagnostic Tests, `NA` denotes Global Service, Mods `26` & `TC` identify Components. Mod `53` indicates Separate RVUs & PFS Amount for Procedures Terminated Before Completion.",
    "status",    "Status Code",                      "Indicates if in Fee Schedule, if Covered, if Separately Payable. Only `A`, `R` and `T` used for Medicare payment.",
    "mult_surg", "Multiple Surgery Indicator",       "Indicates Applicable Payment Adjustment Rule: Mod `51`",
    "flat_vis",  "Flat Rate Visit Fee",              "Contains Flat Visit Fee for Primary Care First Model",
    "nther",     "Non-Facility Therapy Reduction",   "Fee reflects 50% PE payment for Non-facility services",
    "fther",     "Facility Therapy Reduction",       "Fee reflects 50% PE payment for Facility services",
    "fee_nf",    "Non-Facility Fee Schedule Amount", "Non-Facility Pricing Amount",
    "fee_f",     "Facility Fee Schedule Amount",     "Facility Pricing Amount",
    "opps",      "OPPS Indicator",                   "OPPS Payment Cap Determination: `1` = Applies, `9` = Does Not Apply",
    "opps_nf",   "OPPS Non-Facility",                "OPPS Capped Non-Facility Pricing Amount",
    "opps_f",    "OPPS Facility",                    "OPPS Capped Facility Pricing Amount"
  )
}

#' @autoglobal
#'
#' @noRd
dict_rvu <- function() {

  dplyr::tribble(
    ~var,        ~  label,                                         ~description,
    #----          #-----                                          #-----------
    "hcpcs",       "HCPCS Code",                                   "HCPCS Code",
    "description", "Description",                                  "HCPCS Procedure Description",
    "mod",         "Modifier",                                     "Diagnostic Tests, `NA` denotes Global Service, Mods `26` & `TC` identify Components. Mod `53` indicates Separate RVUs & PFS Amount for Procedures Terminated Before Completion.",
    "status",      "Status Code",                                  "Indicates if in Fee Schedule, if Covered, if Separately Payable. Only `A`, `R` and `T` used for Medicare payment.",
    "wrvu",        "Work RVU",                                     "RVUs for Physician Work",
    "nprvu",       "Non-Facility Practice Expense RVU",            "RVUs for Non-Facility Practice Expense",
    "fprvu",       "Facility Practice Expense RVU",                "RVUs for Facility Practice Expense",
    "mrvu",        "Malpractice RVU",                              "RVUs for Malpractice Expense",
    "cf",          "Conversion Factor",                            "Multiplier that Transforms RVUs into Payment Amounts",
    "nprvu_opps",  "Non-Facility PE Used for OPPS Payment Amount", "Non-Facility Practice Expense RVUs for OPPS Payment",
    "fprvu_opps",  "Facility PE Used for OPPS Payment Amount",     "Facility Practice Expense RVUs for OPPS Payment",
    "global",      "Global Days",                                  "Number of Global Days",
    "op_ind",      "Operative Percentage Indicator",               "1 = Has percentages, 0 = Does not have percentages",
    "op_pre",      "Preoperative Percentage",                      "Preoperative % of Global Package",
    "op_intra",    "Intraoperative Percentage",                    "Intraoperative % of Global Package, including Postoperative Work in Hospital",
    "op_post",     "Postoperative Percentage",                     "Postoperative % of Global Package, Provided in Office, Post-Discharge",
    "pctc",        "PCTC Indicator",                               "PCTC Payment Adjustment",
    "mult_proc",   "Multiple Procedure Indicator",                 "Multiple Procedures (Mod 51) Payment Adjustment",
    "surg_bilat",  "Bilateral Surgery Indicator",                  "Bilateral Procedure (Mod 50) Payment Adjustment",
    "surg_asst",   "Assistant Surgery Indicator",                  "Assistant at Surgery (Mods 80, 81, 82, or AS) Payment Adjustment",
    "surg_co",     "Co-Surgery Indicator",                         "Co-surgeons (Mod 62) Payment Adjustment",
    "surg_team",   "Team Surgery Indicator",                       "Team Surgeons (Mod 66) Payment Adjustment",
    "endo",        "Endoscopic Base Code",                         "Endoscopic Base Code for HCPCS with Multiple Surgery indicator **3**",
    "supvis",      "Physician Supervision Indicator",              "Physician Supervision Level Required for Service",
    "dximg",       "Diagnostic Imaging Family Indicator",          "Diagnostic Service Family for HCPCS with Multiple Procedure indicator **4**",
    "unused",      "Not Used for Medicare Payment",                "Whether Code used for Medicare Payment",
    "rare",        "Rarely/Never Performed",                       "Procedure rarely/never performed in: `00` (Neither), `01` (Facility), `10` (Non-Facility), `11` (Both)"
  )
}



