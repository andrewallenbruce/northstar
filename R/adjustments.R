#' Search Adjustment Codes
#'
#' CARC and RARC Codes
#'
#' @details Claim Adjustment Reason Codes:
#'
#'   _X12 External Code Source 139_
#'
#'   These codes describe why a claim or service line was paid differently than
#'   it was billed and generally assign responsibility for the adjustment
#'   amounts.
#'
#'   The Claim Adjustment *Group Codes* are internal to the X12 standard. The
#'   format is always two alpha characters:
#' - **CO**: Contractual Obligations
#' - **CR**: Corrections and Reversals
#' - **OA**: Other Adjustments
#' - **PI**: Payer Initiated Reductions
#' - **PR**: Patient Responsibility
#'
#' @details Remittance Advice Remark Codes:
#'
#'   _X12 External Code Source 411_
#'
#'   Remittance Advice Remark Codes (RARCs) are used to provide additional
#'   explanation for an adjustment already described by a Claim Adjustment
#'   Reason Code (CARC) or to convey information about remittance processing.
#'
#'   There are two types of RARCs: **Supplemental** and **Informational**:
#'
#'   The majority of RARCs are *supplemental* and, as such, are generally
#'   referred to as RARCs without further distinction. Supplemental RARCs
#'   provide additional explanation for an adjustment already described by a
#'   CARC.
#'
#'   The second type is *informational*; these are all prefaced with `Alert:`
#'   and are referred to as Alerts. They are used to convey information about
#'   remittance processing and are *never* related to a specific adjustment or
#'   CARC.
#'
#' @param adj_code `<chr>` vector of Adjustment codes; default is `NULL`
#'
#' @param adj_type `<chr>` type of Adjustment code; `Group`, `CARC`, `RARC`;
#'   default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_adjustments(adj_type = "Group")
#'
#' search_adjustments(adj_type = "CARC")
#'
#' search_adjustments(adj_type = "RARC")
#'
#' @autoglobal
#'
#' @family HIPAA Code Standards
#'
#' @export
search_adjustments <- function(adj_code = NULL, adj_type = NULL, ...) {

  adj_type <- if (!is.null(adj_type)) match.arg(
    adj_type,
    c("Group", "CARC", "RARC"))

  adj <- get_pin("adj_codes")

  adj <- fuimus::search_in_if(adj, adj$adj_type, adj_type)
  adj <- fuimus::search_in_if(adj, adj$adj_code, adj_code)

  return(.add_class(adj))
}

#' Search Denial Types
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_denials()
#'
#' @autoglobal
#'
#' @export
search_denials <- function(...) {
  list(
    site = get_pin("denials_site") |> .add_class(),
    ext = get_pin("denials_extract") |> .add_class()
  )

}

#' Make Adjustment Code Trie
#'
#' @examples
#' adj_trie()
#'
#' @noRd
#'
#' @autoglobal
adj_trie <- function() {

  ad <- get_pin("adj_codes")

  triebeard::trie(
    keys = ad$adj_code,
    values = ad$adj_description)

}

#' Assign Adjustment Codes
#'
#' @param code `<chr>` vector of adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @param include_keys `<lgl>` include keys in output; default is `FALSE`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' x <- c("CO-253", "OA-23", "PI-185", "-45")
#'
#' assign_adjustments(x)
#'
#' assign_adjustments(x, include_keys = TRUE)
#'
#' dplyr::tibble(code = x, desc = assign_adjustments(code))
#'
#' dplyr::tibble(code = x) |>
#'   dplyr::mutate(desc = purrr::map(code, assign_adjustments))
#'
#' purrr::map_df(x, assign_adjustments)
#'
#' purrr::map_df(x, assign_adjustments, include_keys = TRUE)
#'
#' dplyr::tibble(code = x, purrr::map_dfr(code, assign_adjustments))
#'
#' assign_adjustments(x, include_keys = TRUE) |>
#'   as.data.frame() |>
#'   dplyr::tibble()
#'
#' @export
#'
#' @autoglobal
assign_adjustments <- function(code, include_keys = FALSE, ...) {

  cd <- carc_add_dash(code)

  cd <- stringfish::sf_split(cd, "-")

  ln <- seq_along(cd)

  tr <- adj_trie()

  group <- triebeard::longest_match(
    tr,
    purrr::map_chr(
      ln,
      ~getElement(cd, .x)[1]
      ),
    include_keys = include_keys
    )
  desc  <- triebeard::longest_match(
    tr,
    purrr::map_chr(
      ln,
      ~getElement(cd, .x)[2]
      ),
    include_keys = include_keys
    )

  dplyr::tibble(
    adj_code  = code,
    adj_group = group,
    adj_desc  = desc
  )
}

#' Format CARC Codes for Identification
#'
#' @param x `<chr>` vector of CARC adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @param placeholder `<chr>` placeholder string for missing elements of CARC
#'   codes; default is `||`
#'
#' @template returns
#'
#' @examples
#' carc_add_dash(c("- 253", "OA-23", "PI-", "-45 ", "OA23"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
carc_add_dash <- \(x, placeholder = "||") {

  # Remove all whitespace
  x <- gsub(" ", "", x)

  dplyr::case_when(
    # "OA" -> "OA-||"
    stringr::str_detect(x,
      stringr::regex("^[ACIOPR]{2}-?$")) ~
      stringr::str_c(x, "-", placeholder),

    # "OA-" -> "OA-||"
    stringr::str_detect(x,
      stringr::regex("^[ACIOPR]{2}-$")) ~
      stringr::str_c(x, placeholder),

    # "OA" -> "OA-||"
    stringr::str_detect(x,
                        stringr::regex("^[ACIOPR]{2}$")) ~
      stringr::str_c(x, "-", placeholder),

    # "123" -> "||-123"
    stringr::str_detect(x,
      stringr::regex("^[0-9]{1,3}$")) ~
      stringr::str_c(placeholder, "-", x),

    # "-123" -> "||-123"
    stringr::str_detect(x,
      stringr::regex("^-[0-9]{1,3}$")) ~
      stringr::str_c(placeholder, x),

    # "A123" -> "||-A123"
    stringr::str_detect(x,
      stringr::regex("^[ABDPWY]{1}[0-9]{1,2}$")) ~
      stringr::str_c(placeholder, "-", x),

    # "-A123" -> "||-A123"
    stringr::str_detect(x,
      stringr::regex("^-[ABDPWY]{1}[0-9]{1,2}$")) ~
      stringr::str_c(placeholder, x),

    ## RARC Codes:
    # stringr::str_detect(x,
    # stringr::regex("^[A-Z]{1,2}[0-9]{2,3}$")
    # ) == TRUE ~ stringr::str_c(placeholder, x),

    .default = x
  )
}

#' Validate CARC Codes
#'
#' @param x `<chr>` vector of CARC adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @template returns
#'
#' @examples
#' x <- c("- 253", "OA-23", "PI-", "-45 ", "OA23")
#'
#' is_carc_code(x)
#'
#' x[which(is_carc_code(x))]
#'
#' @autoglobal
#'
#' @export
is_carc_full <- function(x) {

  stringr::str_detect(
    gsub(" ", "", x),
    stringr::regex(
      "^[COP]{1}[AIOR]{1}-?[ABDPWY123456789]{1,3}$"))

}

#' Validate CARC Codes
#'
#' @param x `<chr>` vector of CARC adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @template returns
#'
#' @examples
#' x <- c("- 253", "OA-23", "PI-", "-45 ", "OA23")
#'
#' is_carc_code(x)
#'
#' x[which(is_carc_code(x))]
#'
#' @autoglobal
#'
#' @export
is_carc_code <- function(x) {

  stringr::str_detect(gsub(" ", "", x),
    stringr::regex("^[ACIOPR]{2}-?[ABDPW]?[0-9]{1,3}$"))

}

#' Validate CARC Group Codes
#'
#' @param x `<chr>` vector of CARC adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @template returns
#'
#' @examples
#' is_carc_group(c("- 253", "OA-23", "PI-", "-45 ", "OA23"))
#'
#' @autoglobal
#'
#' @export
is_carc_group <- function(x) {

  stringr::str_detect(
    gsub(" ", "", x),
    stringr::regex(
      # "^[ACIOPR]{2}-?$"
      "^[COP][AIOR]-?[ABDPWY123456789]$"
      )
    )
}
