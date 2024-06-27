#' Level I and II HCPCS Modifiers
#'
#' A modifier provides the means to report or indicate that a service or
#' procedure that has been performed has been altered by some specific
#' circumstance but not changed in its definition or code.
#'
#' Modifiers also enable health care professionals to effectively respond to
#' payment policy requirements established by other entities.
#'
#' @param mod_code `<chr>` vector of 2-character HCPCS modifiers; default is
#'   `NULL`
#'
#' @param mod_type `<chr>` Modifier type, one of `HCPCS`, `CPT`,
#'   `Anesthesia`, or `Performance Measure`; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_modifiers(mod_code = c("25", "59"))
#'
#' search_modifiers(mod_type = "CPT")
#'
#' @autoglobal
#'
#' @family HIPAA Code Standards
#'
#' @export
search_modifiers <- function(mod_code = NULL, mod_type = NULL, ...) {

  md <- get_pin("modifiers")
  md <- fuimus::search_in_if(md, md$mod_type, mod_type)
  md <- fuimus::search_in_if(md, md$mod_code, mod_code)
  return(.add_class(md))

}

#' Level II HCPCS Codes
#'
#' The Healthcare Common Procedure Coding System (HCPCS) is a collection of
#' codes that represent procedures, supplies, products and services which may be
#' provided to Medicare beneficiaries and to individuals enrolled in private
#' health insurance programs. The codes are divided into two levels, or groups,
#' as described below.
#'
#'
#'    * Level I Codes and descriptors copyrighted by the American Medical
#'    Association's current procedural terminology, fourth edition (CPT-4). These
#'    are 5 position numeric codes representing physician and non-physician
#'    services.
#'
#'    * Level II Includes codes and descriptors copyrighted by the American Dental
#'    Association's current dental terminology, (CDT-2023). These are 5 position
#'    alpha-numeric codes comprising the d series.
#'
#'
#' All level II codes and descriptors are approved and maintained jointly by the
#' alpha-numeric editorial panel (consisting of CMS, the Health Insurance
#' Association of America, and the Blue Cross and Blue Shield Association).
#'
#' These are 5 position alpha-numeric codes representing primarily items and
#' non-physician services that are not represented in the level I codes.
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_hcpcs(hcpcs_code = c("C9039", "J1835"))
#'
#' @autoglobal
#'
#' @family HIPAA Code Standards
#'
#' @export
search_hcpcs <- function(hcpcs_code = NULL, ...) {

  lv <- get_pin("hcpcs_lvl2")
  lv <- fuimus::search_in_if(lv, lv$hcpcs_code, hcpcs_code)
  return(.add_class(lv))

}

#' Place of Service (POS) Codes
#'
#' Place of Service Codes are two-digit codes placed on health care professional
#' claims to indicate the setting in which a service was provided. The Centers
#' for Medicare & Medicaid Services (CMS) maintain POS codes used throughout the
#' health care industry.
#'
#' This code set is required for use in the implementation guide adopted as the
#' national standard for electronic transmission of professional health care
#' claims under the provisions of the Health Insurance Portability and
#' Accountability Act of 1996 (HIPAA).
#'
#' HIPAA directed the Secretary of HHS to adopt national standards for
#' electronic transactions. These standard transactions require all health plans
#' and providers to use standard code sets to populate data elements in each
#' transaction.
#'
#' The Transaction and Code Set Rule adopted the ASC X12N-837 Health Care Claim:
#' Professional, volumes 1 and 2, version 4010, as the standard for electronic
#' submission of professional claims.
#'
#' This standard names the POS code set currently maintained by CMS as the code
#' set to be used for describing sites of service in such claims. POS
#' information is often needed to determine the acceptability of direct billing
#' of Medicare, Medicaid and private insurance services provided by a given
#' provider.
#'
#' @param pos_code `<chr>` vector of 2-character Place of Service codes; default
#'   is `NULL`
#'
#' @param pos_type `<chr>` Place of Service type, one of `Facility` or
#'   `Non-Facility`; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_pos(pos_code = c("11", "21"))
#'
#' search_pos(pos_type = "Facility")
#'
#' @autoglobal
#'
#' @family HIPAA Code Standards
#'
#' @export
search_pos <- function(pos_code = NULL, pos_type = NULL, ...) {

  pos <- get_pin("pos_codes")
  pos <- fuimus::search_in_if(pos, pos$pos_type, pos_type)
  pos <- fuimus::search_in_if(pos, pos$pos_code, pos_code)
  return(.add_class(pos))
}


#' Restructured BETOS Classifications
#'
#' [search_rbcs()] allows the user to group HCPCS codes into clinically
#' meaningful categories based on the original _Berenson-Eggers Type of Service_
#' (BETOS) classification.
#'
#' @section From BETOS to RBCS:
#'
#' The Restructured BETOS Classification System (RBCS) is a taxonomy that allows
#' researchers to group Medicare Part B healthcare service codes into clinically
#' meaningful categories and subcategories.
#'
#' Based on the original Berenson-Eggers Type of Service (BETOS) classification
#' created in the 1980s, it includes notable updates such as Part B non-physician
#' services and undergoes annual updates by a technical expert panel of
#' researchers and clinicians.
#'
#' The general framework for grouping service codes into the new RBCS taxonomy
#' largely follows the same structure of BETOS. Like BETOS, the RBCS groups
#' HCPCS codes into categories, subcategories, and families – with categories
#' as the most aggregate level and families as the more granular level.
#'
#' All Medicare Part B service codes, including non-physician services, are
#' assigned to a 6-character RBCS taxonomy code.
#'
#' @section Links:
#'
#' + [Restructured BETOS Classification System](https://data.cms.gov/provider-summary-by-type-of-service/provider-service-classifications/restructured-betos-classification-system)
#' + [RBCS Data Dictionary](https://data.cms.gov/resources/restructured-betos-classification-system-data-dictionary)
#'
#' @section Update Frequency: Annually
#'
#' @template args-hcpcs
#'
#' @param category `<chr>` vector of RBCS categories:
#' + `Procedure` (n = 6920)
#' + `Test` (n = 3015)
#' + `DME` (n = 2971)
#' + `Treatment` (n = 1795)
#' + `Imaging` (n = 1097)
#' + `E&M` (n = 695)
#' + `Anesthesia` (n = 307)
#' + `Other` (n = 233)
#'
#' @param subcategory `<chr>` vector of RBCS subcategories (53 unique)
#'
#' @param family `<chr>` vector of RBCS families (178 unique)
#'
#' @param procedure `<chr>` Procedure Type:
#' * `Major` (n = 3676)
#' * `Non-Procedure` (n = 10113)
#' * `Other` (n = 3244)
#'
#' @param concatenate `<lgl>` Concatenate output, default is `TRUE`
#'
#' @template args-dots
#'
#' @returns A [tibble][tibble::tibble-package] with the columns:
#'
#' |**Column**          |**Description**                              |
#' |:-------------------|:--------------------------------------------|
#' |`hcpcs`             |HCPCS or CPT code                            |
#' |`rbcs`              |RBCS Identifier                              |
#' |`rbcs_category`     |RBCS Category                                |
#' |`rbcs_subcategory`  |RBCS Subcategory                             |
#' |`rbcs_family`       |RBCS Family                                  |
#' |`rbcs_procedure`    |RBCS Major Procedure Indicator               |
#' |`date_hcpcs_add`    |Date HCPCS Code was added                    |
#' |`date_hcpcs_end`    |Date HCPCS Code was no longer effective      |
#' |`date_rbcs_assign`  |Earliest Date that the RBCS ID was effective |
#'
#' @examples
#' search_rbcs(hcpcs_code = c("J9264", "39503", "43116", "70170", "0001U"))
#'
#' search_rbcs(hcpcs_code = "0001U")
#'
#' @autoglobal
#'
#' @family HIPAA Code Standards
#'
#' @export
search_rbcs <- function(hcpcs_code  = NULL,
                        category    = NULL,
                        subcategory = NULL,
                        family      = NULL,
                        procedure   = NULL,
                        concatenate = FALSE,
                        ...) {

  rb <- get_pin("rbcs")

  rb <- fuimus::search_in_if(rb, rb$hcpcs_code, hcpcs_code)
  rb <- fuimus::search_in_if(rb, rb$rbcs_family, family)
  rb <- fuimus::search_in_if(rb, rb$rbcs_subcategory, subcategory)

  if (!is.null(procedure)) {

    procedure <- rlang::arg_match(
      procedure,
      c("Major", "Non-Procedure", "Other"),
      multiple = TRUE)

    rb <- fuimus::search_in(rb, rb$rbcs_procedure, procedure)
  }

  if (!is.null(category)) {

    category <- rlang::arg_match(
      category,
      c("Procedure", "Test", "DME", "Treatment",
        "Imaging", "E&M", "Anesthesia", "Other"),
      multiple = TRUE)

    rb <- fuimus::search_in(rb, rb$rbcs_category, category)
  }

  if (concatenate) {

    rb <- tidyr::unite(
      rb,
      "rbcs_category",
      c(rbcs_procedure, rbcs_category),
      sep = " ") |>
      tidyr::unite(
        "rbcs_family",
        c(rbcs_subcategory, rbcs_family),
        sep = ": ",
        na.rm = TRUE) |>
      dplyr::select(
        hcpcs_code,
        rbcs_category,
        rbcs_family)
  }
  return(.add_class(rb))
}
