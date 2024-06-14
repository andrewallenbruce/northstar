#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
remove_quiet <- function(df) {

  janitor::remove_empty(
    df,
    which = c("rows", "cols")
  )
}

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
    local  = pins::board_folder(
             fs::path_package(
               "extdata/pins",
               package = "northstar")
             ),
    remote = pins::board_url(
      fuimus::gh_raw(
      "andrewallenbruce/northstar/master/inst/extdata/pins/")
      )
    )
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

#' Search in data frame column if search term is not `NULL`
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param dfcol A `<character>` or `<symbol>` specifying the column to search in
#'
#' @param search A `<character>` or `<symbol>` specifying the search term
#'
#' @param args A `<character>` vector of argument options; default is `NULL`
#'
#' @param multiple A `<logical>` indicating if multiple `search` args are
#'   allowed; default is `FALSE`
#'
#' @returns A `<data.frame>` or `<tibble>`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10, z = letters[1:10])
#'
#' search_in_if_args(df = x, dfcol = x$z, search = c("a", "j"))
#'
#' search_in_if_args(df = x, dfcol = x$z, search = NULL)
#'
#' search_in_if_args(df = x,
#'                   dfcol = x$z,
#'                   search = c("a", "j"),
#'                   args = c("a", "j"),
#'                   multiple = TRUE)
#'
#' try(search_in_if_args(df = x,
#'                       dfcol = x$z,
#'                       search = c("a", "j"),
#'                       args = c("a", "z"),
#'                       multiple = FALSE))
#'
#' @autoglobal
#'
#' @export
search_in_if_args <- function(df,
                              dfcol,
                              search,
                              args = NULL,
                              multiple = FALSE) {

  if (!is.null(search)) {

    if (!is.null(args)) {
      search <- rlang::arg_match(
        arg = search,
        values = args,
        multiple = multiple)
    }

    vctrs::vec_slice(df,
    vctrs::vec_in(dfcol,
    collapse::funique(search)))

  } else {
    df
    }
}

#' @autoglobal
#'
#' @noRd
null_if_empty <- function(x) {
  if (vctrs::vec_is_empty(x)) NULL else x
}

#' @autoglobal
#' @noRd
col_lb <- function(output = c("md", "df"), type = c("pfs", "rvu")) {

  output <- match.arg(output)
  type <- match.arg(type)

  if (type == "pfs") {res <- pfs_lb()}
  if (type == "rvu") {res <- rvu_lb()}

  if (output == "df") {return(res)}

  if (output == "md") {
    return(
      res |>
        dplyr::mutate(var         = gluedown::md_code(var),
                      label       = gluedown::md_bold(label),
                      description = gluedown::md_hardline(description))
    )
  }
}

#' @autoglobal
#' @noRd
pfs_lb <- function() {
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
#' @noRd
rvu_lb <- function() {
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



