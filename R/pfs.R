#' HCPCS Relative Value Units (RVUs)
#'
#' Physician Fee Schedule Relative Value File
#'
#' @template args-hcpcs
#'
#' @param type `<chr>` part of RVU file to return; `amt` or `ind`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_rvus(hcpcs_code = c("95907", "78140", "32820", "61575"))
#'
#' search_rvus(hcpcs_code = "95907", type = "amt")
#'
#' search_rvus(hcpcs_code = "95907", type = "ind")
#'
#' @autoglobal
#'
#' @family Physician Fee Schedule Sources
#'
#' @export
search_rvus <- function(hcpcs_code = NULL, type = c("amt", "ind"), ...) {

  type <- match.arg(type)

  rv <- switch(
    type,
    amt = get_pin("pfs_rvu_amt"),
    ind = get_pin("pfs_rvu_ind")
  )

  rv <- fuimus::search_in_if(rv, rv$hcpcs_code, hcpcs_code)
  return(.add_class(rv))
}

#' Geographic Practice Cost Indices (GPCIs)
#'
#' A geographic practice cost index (GPCI) has been established for every
#' Medicare payment locality for each of the three components of a procedure's
#' relative value unit (RVU): work, practice expense, and malpractice. The GPCI
#' reflects the relative costs of physician work, practice expense, and
#' malpractice insurance in an area compared to the national average costs for
#' each component.
#'
#' The GPCIs are applied in the calculation of a Medicare payment schedule
#' amount by multiplying the RVU for each component of a procedure by the GPCI
#' for the locality in which the service is furnished.
#'
#' GPCIs allow for considerably less variation in physicians' costs of practice
#' than under the previous reasonable charge system. The GPCIs are based on the
#' premise that the costs of practice resources are similar across the country,
#' but that the costs of operating a medical practice can vary significantly
#' from one area to another.
#'
#'
#' ## Physician Work (Cost-of-Living)
#'
#' The work GPCIs are designed to reflect the relative cost of physician labor
#' by Medicare locality and are measured from salary information of individuals
#' with higher education.
#'
#' To address physician workforce needs, Congress established a 1.0 floor for
#' for physician work RVUs and incentivizes Alaska by establishing a permanent
#' 1.5 floor (the highest work GPCI.) The remaining work GPCIs range from 1.002
#' to 1.1. Because the cost-of-living GPCI (work) accounts for only one quarter
#' of geographic differences, the range is small.
#'
#'
#' ## Practice Expense
#'
#' The practice expense GPCIs are designed to measure geographic variation in
#' the prices of inputs to medical practice. They are comprised of four
#' component indices: employee wages, purchased services, office rent and
#' equipment, supplies, and other miscellaneous expenses.
#'
#' Congress established the following states as "Frontier States": Montana,
#' Nevada, North Dakota, South Dakota, and Wyoming. The practice expense GPCIs
#' for these states are set at a 1.0 floor. The remaining PE GPCIs range from
#' 0.852 (Mississippi) to 1.435 (San Jose, California.)
#'
#'
#' ## Malpractice Expense
#'
#' The Professional Liability Insurance (PLI), which Medicare regulations refer
#' to as the malpractice GPCI, account for geographic differences in PLI premium
#' costs. These have the most variation of the three GPCIs, ranging from 0.3
#' (Minnesota) to 2.5 (Miami).
#'
#' 36 states and territories have a single set of GPCIs. The remaining 14 states
#' differentiate GPCIs by urban and rural areas. California has the most
#' differentiation, with 29 different geographic localities.
#'
#' ## Geographic Adjustment Factor (GAF)
#'
#' The three GPCI components can be combined into a composite GPCI (or GAF) by
#' weighting each by the share of Medicare payments accounted for by the work,
#' practice cost, and PLI components. GAF indicates how Medicare payments in a
#' locality differ from the national average (the average being 1.00).
#'
#' GAFs are not used in the calculation of the Medicare payment schedule amount,
#' but are a useful measure to illustrate the overall effect of geographic
#' adjustments under the PFS across Medicare fee schedule areas.
#'
#' @template args-mac
#'
#' @template args-state
#'
#' @template args-locality
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_gpcis(state = "GA", locality = "01", mac = "10212")
#'
#' @autoglobal
#'
#' @family Physician Fee Schedule Sources
#'
#' @export
search_gpcis <- function(mac = NULL,
                         state = NULL,
                         locality = NULL,
                         ...) {

  gp <- get_pin("pfs_gpci")
  gp <- fuimus::search_in_if(gp, gp$state, state)
  gp <- fuimus::search_in_if(gp, gp$mac, mac)
  gp <- fuimus::search_in_if(gp, gp$locality, locality)
  return(.add_class(gp))
}

#' Physician Fee Schedule Payment Amount File
#'
#' @template args-hcpcs
#'
#' @template args-mac
#'
#' @template args-locality
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_payment(hcpcs_code = c("39503", "43116", "33935", "11646"),
#'                locality = "01",
#'                mac = "10212")
#'
#' @autoglobal
#'
#' @family Physician Fee Schedule Sources
#'
#' @export
search_payment <- function(hcpcs_code = NULL,
                           mac = NULL,
                           locality = NULL,
                           ...) {
  pmt <- get_pin("pfs_pmt")
  pmt <- fuimus::search_in_if(pmt, pmt$hcpcs_code, hcpcs_code)
  pmt <- fuimus::search_in_if(pmt, pmt$mac, mac)
  pmt <- fuimus::search_in_if(pmt, pmt$locality, locality)
  return(.add_class(pmt))
}

#' Anesthesia Conversion Factors
#'
#' @template args-mac
#'
#' @template args-locality
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_anesthesia(locality = "01", mac = "10212")
#'
#' @autoglobal
#'
#' @family Physician Fee Schedule Sources
#'
#' @export
search_anesthesia <- function(mac      = NULL,
                              locality = NULL,
                              ...) {

  an <- get_pin("pfs_anes")
  an <- fuimus::search_in_if(an, an$mac, mac)
  an <- fuimus::search_in_if(an, an$locality, locality)
  return(.add_class(an))
}

#' Outpatient Prospective Payment System (OPPS) Capitations
#'
#' Contains the payment amounts after the application of the OPPS-based payment
#' caps, except for carrier priced codes. For carrier price codes, the field
#' only contains the OPPS-based payment caps. Carrier prices cannot exceed the
#' OPPS-based payment caps.
#'
#' @template args-hcpcs
#'
#' @template args-mac
#'
#' @template args-locality
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_opps(hcpcs_code = "0633T")
#'
#' @autoglobal
#'
#' @family Physician Fee Schedule Sources
#'
#' @export
search_opps <- function(hcpcs_code = NULL,
                        mac = NULL,
                        locality = NULL,
                        ...) {

  op <- get_pin("pfs_opps")
  op <- fuimus::search_in_if(op, op$hcpcs_code, hcpcs_code)
  op <- fuimus::search_in_if(op, op$mac, mac)
  op <- fuimus::search_in_if(op, op$locality, locality)

  return(.add_class(op))
}
