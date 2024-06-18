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
#' @param type `<chr>` type of Adjustment code; `group` (default), `carc`,
#'   `rarc`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_adjustments("group")
#'
#' search_adjustments("carc")
#'
#' search_adjustments("rarc")
#'
#' @autoglobal
#'
#' @export
search_adjustments <- function(type = c("group", "carc", "rarc"), ...) {

  type <- match.arg(type)

  ad <- switch(
    type,
    group = get_pin("adj_group"),
    carc  = get_pin("adj_carc"),
    rarc  = get_pin("adj_rarc")
  ) |>
    .add_class()

  return(ad)
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

  gr <- get_pin("adj_group")
  cc <- get_pin("adj_carc")
  rc <- get_pin("adj_rarc")

  triebeard::trie(
    keys = c(gr$code, cc$code, rc$code),
    values = c(gr$description, cc$description, rc$description))

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
#' dplyr::tibble(code = x,
#'               desc = list(assign_adjustments(code)))
#'
#' dplyr::tibble(code = x) |>
#'   dplyr::mutate(desc = purrr::map(code, assign_adjustments))
#'
#' purrr::map_df(x, assign_adjustments)
#'
#' purrr::map_df(x, assign_adjustments, include_keys = TRUE)
#'
#' dplyr::tibble(adj_code = x,
#'   purrr::map_dfr(adj_code, assign_adjustments))
#'
#' assign_adjustments(
#'   carc_add_dash(c("CO-253", "OA-23", "PI-185", "-45")),
#'     include_keys = TRUE) |>
#'     as.data.frame() |>
#'     dplyr::tibble()
#'
#' @export
#'
#' @autoglobal
assign_adjustments <- function(code, include_keys = FALSE, ...) {

  # TODO if code = "- 29" or "29"

  # cd <- strsplit(c(code), "[-]")
  cd <- stringfish::sf_split(code, "-")
  ln <- seq_along(cd)
  tr <- adj_trie()

  list(
    adj_group_desc = triebeard::longest_match(
      tr,
      purrr::map_chr(
        ln, ~getElement(cd, .x)[1]),
      include_keys = include_keys
      ),
    adj_code_desc = triebeard::longest_match(tr,
      purrr::map_chr(ln, ~getElement(cd, .x)[2]),
      include_keys = include_keys
      )
    )
}

#' Format CARC Codes for Identification
#'
#' @param x `<chr>` vector of CARC adjustment codes; should be of the form
#'   `GROUP-CARC`, where `GROUP` is two letters, followed by a dash (`-`) and
#'   `CARC` is a two-to-three character alphanumeric string.
#'
#' @param placeholder `<chr>` placeholder text for missing elements of CARC
#'   codes; default is `||`
#'
#' @template returns
#'
#' @examples
#' carc_add_dash(c("- 253", "OA-23", "PI-", "-45 "))
#'
#' @autoglobal
#'
#' @export
carc_add_dash <- function(x, placeholder = "||") {

  x <- gsub(" ", "", x)

  dplyr::case_when(

    ## CARC Groups -----------------
    # beginning of string, 2 letters
    stringr::str_detect(
      x,
      stringr::regex("^[A-Z]{2}$")
      ) == TRUE ~ stringr::str_c(
        x,
        "-",
        placeholder
        ),

    # beginning of string, 2 letters, dash
    stringr::str_detect(
      x,
      stringr::regex("^[A-Z]{2}-$")
      ) == TRUE ~ stringr::str_c(
        x,
        placeholder
        ),

    ## CARC Codes --------------------
    # beginning of string, 1-3 numbers
    stringr::str_detect(
      x,
      stringr::regex("^[0-9]{1,3}$")
      ) == TRUE ~ stringr::str_c(
        placeholder,
        "-",
        x
        ),

    # beginning of string, dash, 1-3 numbers
    stringr::str_detect(
      x,
      stringr::regex("^-[0-9]{1,3}$")
      ) == TRUE ~ stringr::str_c(
        placeholder,
        x
        ),

    # beginning of string, 1 letter, 2-3 numbers
    stringr::str_detect(
      x,
      stringr::regex("^[ABDPWY]{1}[0-9]{1,2}$")
      ) == TRUE ~ stringr::str_c(
        placeholder,
        "-",
        x
        ),

    # beginning of string, dash, 1 letter, 1-2 numbers
    stringr::str_detect(
      x,
      stringr::regex("^-[ABDPWY]{1}[0-9]{1,2}$")
      ) == TRUE ~ stringr::str_c(
        placeholder,
        x
        ),

    ## RARC Codes:
    # stringr::str_detect(x,
    # stringr::regex("^[A-Z]{1,2}[0-9]{2,3}$")
    # ) == TRUE ~ stringr::str_c(placeholder, "-", x),
    # stringr::str_detect(x,
    # stringr::regex("^[A-Z]{1,2}[0-9]{2,3}$")
    # ) == TRUE ~ stringr::str_c(placeholder, x),

    .default = x
  )
}
