#' Check that input is 5 characters long
#'
#' @param x `<chr>` string
#'
#' @inheritParams rlang::args_error_context
#'
#' @autoglobal
#'
#' @noRd
check_nchars <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (any(stringfish::sf_nchar(x) != 5L, na.rm = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} must be 5 characters long.",
      arg = arg,
      call = call)
  }
}

#' Validate HCPCS Code
#'
#' A valid HCPCS code is:
#'
#'    * 5 characters long
#'    * Begins with one of `[A-CEGHJ-MP-V0-9]`
#'    * Followed by any 3 digits, `[0-9]{3}`, and
#'    * Ends with one of `[AFMTU0-9]`
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_valid_hcpcs(x)
#'
#' x[which(is_valid_hcpcs(x))]
#'
#' try(is_valid_hcpcs("1164"))
#'
#' @autoglobal
#'
#' @export
is_valid_hcpcs <- function(hcpcs_code) {
  check_nchars(hcpcs_code)
  hcpcs_code <- stringfish::sf_toupper(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^[A-CEGHJ-MP-V0-9]\\d{3}[AFMTU0-9]$")
}

#' Validate HCPCS Level I (CPT) Code
#'
#' A valid HCPCS Level I code is:
#'
#'    * 5 characters long
#'    * Begins with any 4 digits, `[0-9]{4}`, and
#'    * Ends with one of `[AFMTU0-9]`
#'
#' HCPCS Level I is comprised of CPT (Current Procedural Terminology),
#' a numeric coding system maintained by the American Medical Association
#' (AMA).
#'
#' CPT is a uniform coding system consisting of descriptive terms and
#' identifying codes that are used primarily to identify medical services and
#' procedures furnished by physicians and other health care professionals.
#'
#' These health care professionals use the CPT to identify services and
#' procedures for which they bill public or private health insurance programs.
#'
#' Decisions regarding the addition, deletion, or revision of CPT codes are
#' made by the AMA. The CPT codes are republished and updated annually by the
#' AMA.
#'
#' Level I does not include codes needed to separately report medical items or
#' services that are regularly billed by suppliers other than physicians.
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_hcpcs_level_I(x)
#'
#' x[which(is_hcpcs_level_I(x))]
#'
#' @autoglobal
#'
#' @export
is_hcpcs_level_I <- function(hcpcs_code) {
  is_valid_hcpcs(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^\\d{4}[AFMTU0-9]$")
}

#' Validate HCPCS Level II Code
#'
#' A valid HCPCS Level II code is:
#'
#'    * 5 characters long
#'    * Begins with one of `[A-CEGHJ-MP-V]` and
#'    * Ends with any 4 digits, `[0-9]{4}`
#'
#' Level II of the HCPCS is a standardized coding system that is used primarily
#' to identify products, supplies, and services not included in the Level I CPT
#' codes.
#'
#' These include items such as ambulance services and durable medical equipment,
#' prosthetics, orthotics, and supplies (DMEPOS) when used outside a physician's
#' office.
#'
#' Because Medicare and other insurers cover a variety of services, supplies,
#' and equipment that are not identified by CPT codes, the level II HCPCS codes
#' were established for submitting claims for these items.
#'
#' Level II codes are also referred to as alpha-numeric codes because they
#' consist of a single alphabetical letter followed by 4 numeric digits, while
#' CPT codes are identified using 5 numeric digits.
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_hcpcs_level_II(x)
#'
#' x[which(is_hcpcs_level_II(x))]
#'
#' @autoglobal
#'
#' @export
is_hcpcs_level_II <- function(hcpcs_code) {
  is_valid_hcpcs(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^[A-CEGHJ-MP-V]\\d{4}$")
}

#' Validate HCPCS Level I (CPT) Category I Code
#'
#' A valid CPT Category I code is:
#'
#'    * 5 characters long
#'    * Begins with any 4 digits, `[0-9]{4}`, and
#'    * Ends with one of `[AMU0-9]`
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_cpt_category_I(x)
#'
#' x[which(is_cpt_category_I(x))]
#'
#' @autoglobal
#'
#' @export
is_cpt_category_I <- function(hcpcs_code) {
  is_hcpcs_level_I(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^\\d{4}[AMU0-9]$")
}

#' Validate HCPCS Level I (CPT) Category II Code
#'
#' A valid CPT Category II code is:
#'
#'    * 5 characters long
#'    * Begins with any 4 digits, `[0-9]{4}`, and
#'    * Ends with an `[F]`
#'
#' Category II codes are supplemental tracking codes that are used to track
#' services on claims for performance measurement and are not billing codes.
#'
#' These codes are intended to facilitate data collection about quality of care
#' by coding certain services and/or test results that support performance
#' measures and that have been agreed upon as contributing to good patient care.
#'
#' Some codes in this category may relate to compliance by the health care
#' professional with state or federal law. The use of these codes is optional.
#' The codes are not required for correct coding and may not be used as a
#' substitute for Category I codes.
#'
#' Services/procedures or test results described in this category make use of
#' alpha characters as the 5th character in the string (i.e., 4 digits followed
#' by an alpha character).
#'
#' These digits are not intended to reflect the placement of the code in the
#' regular (Category I) part of the CPT code set.
#'
#' Also, these codes describe components that are typically included in an
#' evaluation and management service or test results that are part of the
#' laboratory test/procedure. Consequently, they do not have a relative value
#' associated with them.
#'
#' @source [AMA Category II Codes](https://www.ama-assn.org/practice-management/cpt/category-ii-codes)
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_cpt_category_II(x)
#'
#' x[which(is_cpt_category_II(x))]
#'
#' @autoglobal
#'
#' @export
is_cpt_category_II <- function(hcpcs_code) {
  is_hcpcs_level_I(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^\\d{4}[F]$")
}

#' Validate HCPCS Level I Category III Code
#'
#' A valid CPT Category III code is:
#'
#'    * 5 characters long
#'    * Begins with any 4 digits, `[0-9]{4}`, and
#'    * Ends with a `[T]`
#'
#' CPT Category III codes are a set of temporary codes that allow data
#' collection for emerging technologies and are used in the the Food and Drug
#' Administration (FDA) approval process.
#'
#' No RVUs are assigned to these codes and payment is based on payer policy. If
#' a Category III code is available, it must be reported in place of a Category
#' I unlisted code.
#'
#' The procedure/service they describe may not meet the following Category I
#' requirements:
#'
#'  - Necessary devices/drugs have received FDA clearance/approval.
#'  - It is performed by many US physicians/QHPs, at a frequency consistent with intended clinical use.
#'  - It is consistent with current medical practice.
#'  - It's clinical efficacy is documented in CPT-approved literature.
#'
#' @source [AMA Category III Codes](https://www.ama-assn.org/practice-management/cpt/category-iii-codes)
#'
#' @param hcpcs_code `<chr>` string
#'
#' @returns `<lgl>` `TRUE` if valid, otherwise `FALSE`
#'
#' @examples
#' x <- c('T1503', 'G0478', '81301', '69641', '0583F', '0779T', NA)
#'
#' is_cpt_category_I(x)
#'
#' x[which(is_cpt_category_III(x))]
#'
#' @autoglobal
#'
#' @export
is_cpt_category_III <- function(hcpcs_code) {
  is_hcpcs_level_I(hcpcs_code)
  stringfish::sf_grepl(hcpcs_code, "^\\d{4}[T]$")
}
