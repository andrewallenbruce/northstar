#' Check if string is valid length of a HCPCS code
#'
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_valid_length("11646")
#'
#' try(is_valid_length("1164"))
#'
#' @export
#'
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
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_level_I("11646")
#'
#' is_level_I("E8015")
#'
#' purrr::map_vec(c("11646", "E8015"), is_level_I)
#'
#' @export
#'
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
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_level_II("E8015")
#'
#' is_level_II("11646")
#'
#' @export
#'
#' @autoglobal
is_level_II <- function(x,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  is_valid_length(x)

  stringr::str_detect(x, stringr::regex("^[A-V]\\d{4}$"))
}

#' Check if code is HCPCS Level 1 Category I (CPT)
#'
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_category_I("11646")
#'
#' is_category_I("1164F")
#'
#' @export
#'
#' @autoglobal
is_category_I <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
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
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_category_II("1164F")
#'
#' is_category_II("11646")
#'
#' @export
#'
#' @autoglobal
is_category_II <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {

  is_level_I(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[F]$"))
}

#' Check if code is HCPCS Level 1 Category III
#'
#' CPT Category III codes are a set of temporary codes that allow data
#' collection for emerging technologies, services, procedures, and service
#' paradigms. These codes are intended to be used for data collection to
#' substantiate widespread usage or to provide documentation for the Food and
#' Drug Administration (FDA) approval process.
#'
#' CPT Category III codes may not conform to one or more of the following CPT
#' Category I code requirements:
#'
#'  - All devices and drugs necessary for performance of the procedure or service have received FDA clearance or approval when such is required for performance of the procedure or service.
#'  - The procedure or service is performed by many physicians or other qualified health care professionals across the United States.
#'  - The procedure or service is performed with frequency consistent with the intended clinical use (i.e., a service for a common condition should have high volume, whereas a service commonly performed for a rare condition may have low volume).
#'  - The procedure or service is consistent with current medical practice.
#'  - The clinical efficacy of the procedure or service is documented in literature that meets the requirements set forth in the CPT code change application.
#'
#' Category III codes are not developed as a result of Panel review of an
#' incomplete proposal, the need for more information, or a lack of CPT
#' Advisory Committee support of a code-change application.
#'
#' CPT Category III codes are not referred to the AMA-Specialty RVS Update
#' Committee (RUC) for valuation because no relative value units (RVUs) are
#' assigned to these codes. Payments for these services or procedures are based
#' on the policies of payers and not on a yearly fee schedule.
#'
#' Category III codes allow data collection for these services or procedures,
#' unlike the use of unlisted codes, which does not offer the opportunity for
#' the collection of specific data. If a Category III code is available, this
#' code must be reported instead of a Category I unlisted code. This is an
#' activity that is critically important in the evaluation of health care
#' delivery and the formation of public and private policy. The use of Category
#' III codes allows physicians and other qualified health care professionals,
#' insurers, health services researchers, and health policy experts to identify
#' emerging technologies, services, procedures, and service paradigms for
#' clinical efficacy, utilization, and outcomes.
#'
#' These codes have an alpha character as the 5th character in the string (i.e.,
#' four digits followed by the letter T). The digits are not intended to reflect
#' the placement of the code in the Category I section of CPT nomenclature.
#' Codes in this section may or may not eventually receive a Category I CPT code.
#' In either case, in general, a given Category III code will be archived five
#' years from the initial publication or extension unless a modification of the
#' archival date is specifically noted at the time of a revision or change to a
#' code (e.g., addition of parenthetical, instructions, reinstatement). Services
#' and procedures described by Category III codes which have been archived after
#' five years, without conversion, must be reported using the Category I unlisted
#' code unless another specific cross-reference is established at the time of archiving.
#'
#' @template args-checks
#'
#' @template returns
#'
#' @examples
#' is_category_III("0074T")
#'
#' is_category_III("11646")
#'
#' @export
#'
#' @autoglobal
is_category_III <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  is_level_I(x)

  stringr::str_detect(x, stringr::regex("^\\d{4}[T]$"))
}
