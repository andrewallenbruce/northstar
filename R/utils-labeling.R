#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_section` column
#' @examples
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_cpt_section(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_cpt_section <- function(df, col) {

  df |>
    dplyr::mutate(cpt_section = dplyr::case_match(
      {{ col }},
      as.character(c(99202:99499)) ~ "E&M",
      as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology",
      as.character(c(10004:69990)) ~ "Surgery",
      as.character(c(70010:79999)) ~ "Radiology",
      as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Path & Lab",
      as.character(c(90281:99199, 99500:99607)) ~ "Medicine"),
      .after = {{ col }})
}

#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `hcpcs_section` column
#' @examples
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_hcpcs_section(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_hcpcs_section <- function(df, col) {

  df |>
    dplyr::mutate(hcpcs_section = dplyr::case_match(
      substr({{ col }}, 1, 1),
      "A" ~ "Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental",
      "B" ~ "Enteral and Parenteral Therapy",
      "C" ~ "Temporary Hospital Outpatient Prospective Payment System",
      "D" ~ "Dental Procedures",
      "E" ~ "Durable Medical Equipment",
      "G" ~ "Temporary Procedures & Professional Services",
      "H" ~ "Rehabilitative Services",
      "J" ~ "Drugs Administered Other Than Oral Method, Chemotherapy Drugs",
      "K" ~ "Temporary Codes for Durable Medical Equipment Regional Carriers",
      "L" ~ "Orthotic/Prosthetic Procedures",
      "M" ~ "Medical Services",
      "P" ~ "Pathology and Laboratory",
      "Q" ~ "Temporary Codes",
      "R" ~ "Diagnostic Radiology Services",
      "S" ~ "Private Payer Codes",
      "T" ~ "State Medicaid Agency Codes",
      "V" ~ "Vision/Hearing Services"
      ),
      .after = {{ col }})
}

#' Add HCPCS Level Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `hcpcs_level` column
#' @examples
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_hcpcs_level(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_hcpcs_level <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(hcpcs_level = dplyr::case_when(
      is_level_I({{ col }}) ~ "I",
      is_level_II({{ col }}) ~ "II"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}

#' Add CPT Category Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_category` column
#' @examplesIf interactive()
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_cpt_category(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_cpt_category <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(cpt_category = dplyr::case_when(
      is_category_I({{ col }}) ~ "I",
      is_category_II({{ col }}) ~ "II",
      is_category_III({{ col }}) ~ "III"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}
