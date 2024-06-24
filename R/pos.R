#' Place of Service (POS) Codes
#'
#' Place of Service Codes are two-digit codes placed on health care professional
#' claims to indicate the setting in which a service was provided. The Centers
#' for Medicare & Medicaid Services (CMS) maintain POS codes used throughout the
#' health care industry.
#'
#' This code set is required for use in the implementation
#' guide adopted as the national standard for electronic transmission of
#' professional health care claims under the provisions of the Health Insurance
#' Portability and Accountability Act of 1996 (HIPAA).
#'
#' HIPAA directed the
#' Secretary of HHS to adopt national standards for electronic transactions.
#' These standard transactions require all health plans and providers to use
#' standard code sets to populate data elements in each transaction.
#'
#' The
#' Transaction and Code Set Rule adopted the ASC X12N-837 Health Care Claim:
#' Professional, volumes 1 and 2, version 4010, as the standard for electronic
#' submission of professional claims. This standard names the POS code set
#' currently maintained by CMS as the code set to be used for describing sites
#' of service in such claims. POS information is often needed to determine the
#' acceptability of direct billing of Medicare, Medicaid and private insurance
#' services provided by a given provider.
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
#' @export
#'
#' @autoglobal
search_pos <- function(pos_code = NULL, pos_type = NULL, ...) {

  pos <- get_pin("pos_codes")
  pos <- fuimus::search_in_if(pos, pos$pos_type, pos_type)
  pos <- fuimus::search_in_if(pos, pos$pos_code, pos_code)
  return(.add_class(pos))
}
