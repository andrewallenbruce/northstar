#' Check if string is valid length of a HCPCS code
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_valid_length("11646")
#'
#' try(is_valid_length("1164"))
#'
#' @export
#' @autoglobal
is_valid_length <- function(x,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {

  if (stringr::str_length(x) != 5L) {
    cli::cli_abort(c(
      "A {.strong HCPCS} code is {.emph 5} characters.",
      "x" = "{.strong {.val {x}}} is {.val {nchar(x)}}."),
      call = call)}

  if (grepl("[[:lower:]]*", x)) {toupper(x)}
}

#' Check if code is HCPCS Level I
#'
#' HCPCS Level I is comprised of CPT (Current Procedural Terminology),
#' a numeric coding system maintained by the American Medical Association
#' (AMA). CPT is a uniform coding system consisting of descriptive terms and
#' identifying codes that are used primarily to identify medical services and
#' procedures furnished by physicians and other health care professionals.
#' These health care professionals use the CPT to identify services and
#' procedures for which they bill public or private health insurance programs.
#'
#' Decisions regarding the addition, deletion, or revision of CPT codes are
#' made by the AMA. The CPT codes are republished and updated annually by the
#' AMA. Level I of the HCPCS, the CPT codes, does not include codes needed to
#' separately report medical items or services that are regularly billed by
#' suppliers other than physicians.
#'
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_level_I("11646")
#'
#' try(is_level_I("E8015"))
#'
#' @export
#' @autoglobal
is_level_I <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[A-Z0-9]$"))
}

#' Check if code is HCPCS Level II
#'
#' HCPCS Level II is a standardized coding system that is used primarily to
#' identify products, supplies, and services not included in the CPT code set
#' jurisdiction, such as ambulance services and durable medical equipment,
#' prosthetics, orthotics, and supplies (DMEPOS) when used outside a
#' physician's office.
#'
#' Level II of the HCPCS is a standardized coding system that is used primarily
#' to identify products, supplies, and services not included in the CPT codes,
#' such as ambulance services and durable medical equipment, prosthetics,
#' orthotics, and supplies (DMEPOS) when used outside a physician's office.
#' Because Medicare and other insurers cover a variety of services, supplies,
#' and equipment that are not identified by CPT codes, the level II HCPCS codes
#' were established for submitting claims for these items. The development and
#' use of level II of the HCPCS began in the 1980's. Level II codes are also
#' referred to as alpha-numeric codes because they consist of a single
#' alphabetical letter followed by 4 numeric digits, while CPT codes are
#' identified using 5 numeric digits.
#'
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_level_II("E8015")
#'
#' try(is_level_II("11646"))
#' @export
#' @autoglobal
is_level_II <- function(x,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^[A-V]\\d{4}$"))
}

#' Check if code is HCPCS Level 1 Category I (CPT)
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_category_I("11646")
#'
#' try(is_category_I("1164F"))
#' @export
#' @autoglobal
is_category_I <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {

  # https://www.johndcook.com/blog/2019/05/05/regex_icd_codes/
  is_level_I(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[A-EG-SU-Z0-9]$"))
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
#' Category II codes are:
#' + Alphanumeric and consist of four digits followed by the letter F
#' + NOT billing codes
#' + Used to track services on claims for performance measurement
#' + Not to be used as a substitute for Category I codes
#'
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_category_II("1164F")
#'
#' try(is_category_II("11646"))
#' @export
#' @autoglobal
is_category_II <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {

  is_level_I(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[F]$"))
}

#' Check if code is HCPCS Level 1 Category III
#'
#' Category III: a temporary set of codes for emerging technologies,
#' services, procedures, and service paradigms.
#'
#' @param x string
#' @param arg string
#' @param call string
#' @return boolean
#' @examples
#' is_category_III("0074T")
#'
#' try(is_category_III("11646"))
#' @export
#' @autoglobal
is_category_III <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  is_level_I(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[T]$"))
}
