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
  return(rv)
}

#' Physician Fee Schedule Geographic Practice Cost Indices
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
#' get_gpcis(state = "GA", locality = "01", mac = "10212")
#'
#' @export
#'
#' @autoglobal
get_gpcis <- function(mac      = NULL,
                      state    = NULL,
                      locality = NULL,
                      ...) {

  gp <- get_pin("gpci")
  gp <- fuimus::search_in_if(gp, gp$state, state)
  gp <- fuimus::search_in_if(gp, gp$mac, mac)
  gp <- fuimus::search_in_if(gp, gp$locality, locality)
  return(gp)
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
  return(pmt)
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
  return(an)
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
