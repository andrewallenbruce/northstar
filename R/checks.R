#' Check if string is valid length of a HCPCS code
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' is_valid_length("11646")
#' @noRd
#' @autoglobal
is_valid_length <- function(x,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {

  if (stringr::str_length(x) != 5L) {
    cli::cli_abort(c(
      "A {.strong HCPCS} code is {.emph 5} characters.",
      "x" = "{.strong {.val {x}}} is {.val {nchar(x)}}."),
      call = call)}
  x
}

#' Check if code is HCPCS Level 1 Category I (CPT)
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' is_category_I("11646")
#' @noRd
#' @autoglobal
is_category_I <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  # https://www.johndcook.com/blog/2019/05/05/regex_icd_codes/
  #
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^\\d{5}$"))
}

#' Check if code is HCPCS Level 1 Category II
#'
#' Category II: Supplemental tracking codes that can be used for performance
#' measurement. These codes are intended to facilitate data collection about
#' quality of care by coding certain services and/or test results that support
#' performance measures and that have been agreed upon as contributing to
#' good patient care.
#'
#' Some codes in this category may relate to compliance by the health care
#' professional with state or federal law. The use of these codes is optional.
#' The codes are not required for correct coding and may not be used as a
#' substitute for Category I codes. Services/procedures or test results
#' described in this category make use of alpha characters as the 5th
#' character in the string (i.e., 4 digits followed by an alpha character).
#'
#' These digits are not intended to reflect the placement of the code in the
#' regular (Category I) part of the CPT code set. Also, these codes describe
#' components that are typically included in an evaluation and management
#' service or test results that are part of the laboratory test/procedure.
#'
#' Consequently, they do not have a relative value associated with them.
#'
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' is_category_II("1164F")
#' @noRd
#' @autoglobal
is_category_II <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[F]$"))
}

#' Check if code is HCPCS Level 1 Category III
#'
#' Category III: a temporary set of codes for emerging technologies,
#' services, procedures, and service paradigms.
#'
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' is_category_III("1164T")
#' @noRd
#' @autoglobal
is_category_III <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[T]$"))
}

#' Check if code is HCPCS Level II
#'
#' HCPCS Level II is a standardized coding system that is used primarily to
#' identify products, supplies, and services not included in the CPT code set
#' jurisdiction, such as ambulance services and durable medical equipment,
#' prosthetics, orthotics, and supplies (DMEPOS) when used outside a
#' physician's office.
#'
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' is_level_II("E8015")
#' @noRd
#' @autoglobal
is_level_II <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^[a-vA-V]\\d{4}$"))
}

#' Check if code is HCPCS Level III
#' @param x string
#' @return boolean
#' @examplesIf interactive()
#' label_hcpcs("A0010")
#' @noRd
#' @autoglobal
case_hcpcs <- function(x) {

  dplyr::case_when(
    is_category_I(x) == TRUE ~ "Category I",
    is_category_II(x) == TRUE ~ "Category II",
    is_category_III(x) == TRUE ~ "Category III",
    is_level_II(x) == TRUE ~ "Level II"
  )

}
