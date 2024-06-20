#' Get HCPCS Codes with RVUs
#'
#' Physician Fee Schedule Relative Value File
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_rvus(hcpcs = c("95907", "78140", "32820", "61575"))
#'
#' @autoglobal
#'
#' @export
get_rvus <- function(hcpcs = NULL, ...) {

  rv <- get_pin("hcpcs_with_rvus")
  rv <- fuimus::search_in_if(rv, rv$hcpcs, hcpcs)
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
#' search_gpci(state = "GA", locality = "01", mac = "10212")
#'
#' @export
#'
#' @autoglobal
search_gpci <- function(mac      = NULL,
                      state    = NULL,
                      locality = NULL,
                      ...) {

  gp <- get_pin("gpci")
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
#' search_payment(hcpcs    = c("39503", "43116", "33935", "11646"),
#'                locality = "01",
#'                mac      = "10212")
#'
#' @autoglobal
#'
#' @export
search_payment <- function(hcpcs    = NULL,
                           mac      = NULL,
                           locality = NULL,
                           ...) {
  pmt <- get_pin("pay_mac_fee")
  pmt <- fuimus::search_in_if(pmt, pmt$hcpcs, hcpcs)
  pmt <- fuimus::search_in_if(pmt, pmt$mac, mac)
  pmt <- fuimus::search_in_if(pmt, pmt$locality, locality)
  return(.add_class(pmt))
}

#' search_anesthesia
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
#' @export
#'
#' @autoglobal
search_anesthesia <- function(mac      = NULL,
                              locality = NULL,
                              ...) {

  an <- get_pin("anesthesia")
  an <- fuimus::search_in_if(an, an$anes_mac, mac)
  an <- fuimus::search_in_if(an, an$anes_locality, locality)
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
#' head(search_opps())
#'
#' @export
#'
#' @autoglobal
search_opps <- function(hcpcs    = NULL,
                        mac      = NULL,
                        locality = NULL,
                        ...) {

  get_pin("opps")

  # op <- get_pin("opps")
  # opp <- fuimus::search_in_if(opp, opp$hcpcs, hcpcs)
  # opp <- fuimus::search_in_if(opp, opp$opps_mac, mac)
  # opp <- fuimus::search_in_if(opp, opp$opps_locality, locality)
  #
  # return(opp)
}
