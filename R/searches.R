#' Physician Fee Schedule Relative Value File
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_rvu(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
#'
#' @autoglobal
#'
#' @export
search_rvu <- function(hcpcs = NULL, ...) {

  rv <- get_pin("rvu")
  rv <- fuimus::search_in_if(rv, rv$hcpcs, hcpcs)
  return(rv)
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
  pmt <- get_pin("pymt")
  pmt <- fuimus::search_in_if(pmt, pmt$hcpcs, hcpcs)
  pmt <- fuimus::search_in_if(pmt, pmt$mac, mac)
  pmt <- fuimus::search_in_if(pmt, pmt$locality, locality)
  return(pmt)
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
#' search_gpci(state = "GA", locality = "01", mac = "10212")
#'
#' @export
#'
#' @autoglobal
search_gpci <- function(mac      = NULL,
                        state    = NULL,
                        locality = NULL,
                        ...) {

  gp <- get_pin("gpci") |> dplyr::rename(area = name)
  gp <- fuimus::search_in_if(gp, gp$state, state)
  gp <- fuimus::search_in_if(gp, gp$mac, mac)
  gp <- fuimus::search_in_if(gp, gp$locality, locality)
  return(gp)
}

#' HCPCS Level II Codes
#'
#' @template args-hcpcs
#'
#' @param columns `<chr>` set of columns returned, default is `limit`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_hcpcs(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
#'
#' @export
#'
#' @autoglobal
search_hcpcs <- function(hcpcs   = NULL,
                         columns = c("limit", "full"),
                         ...) {

  limited <- vctrs::vec_c(
    "hcpcs",
    "description_short",
    "description_long",
    "price",
    "multi_price",
    "labcert",
    "xref",
    "tos",
    "coverage",
    "asc",
    "betos"
  )

  columns <- match.arg(columns)

  lv2 <- switch(
    columns,
    limit  = get_pin("hcpcs")[limited],
    full   = get_pin("hcpcs")
  )

  lv2 <- fuimus::search_in_if(lv2, lv2$hcpcs, hcpcs)

  return(lv2)
}

#' HCPCS Level I (CPT) Codes
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_cpt(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' @export
#'
#' @autoglobal
search_cpt <- function(hcpcs = NULL, ...) {

  cpt <- get_pin("cpt_descriptors")
  cpt <- fuimus::search_in_if(cpt, cpt$hcpcs, hcpcs)
  return(cpt)
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
#' search_opps(hcpcs = "71550", mac = "01112")
#'
#' @export
#'
#' @autoglobal
search_opps <- function(hcpcs    = NULL,
                        mac      = NULL,
                        locality = NULL,
                        ...) {

  opp <- get_pin("opps")
  opp <- fuimus::search_in_if(opp, opp$hcpcs, hcpcs)
  opp <- fuimus::search_in_if(opp, opp$mac, mac)
  opp <- fuimus::search_in_if(opp, opp$locality, locality)

  return(opp)
}
