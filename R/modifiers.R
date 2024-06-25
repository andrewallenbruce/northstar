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
#' @export
#'
#' @autoglobal
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
#' @export
#'
#' @autoglobal
search_hcpcs <- function(hcpcs_code = NULL, ...) {

  lv <- get_pin("hcpcs_lvl2")
  lv <- fuimus::search_in_if(lv, lv$hcpcs_code, hcpcs_code)
  return(.add_class(lv))

}
