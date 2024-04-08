#' Relative Value File
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

  rv <- pins::pin_read(mount_board(), "rvu")

  rv <- search_if(rv, rv$hcpcs, hcpcs)

  return(rv)
}

#' Payment Amount File
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

  pmt <- pins::pin_read(mount_board(), "pymt")

  pmt <- search_if(pmt, pmt$hcpcs, hcpcs)

  pmt <- search_if(pmt, pmt$mac, mac)

  pmt <- search_if(pmt, pmt$locality, locality)

  return(pmt)
}

#' Geographic Practice Cost Indices
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

  gp <- pins::pin_read(mount_board(), "gpci") |>
    dplyr::rename(area = name)

  gp <- search_if(gp, gp$state, state)

  gp <- search_if(gp, gp$mac, mac)

  gp <- search_if(gp, gp$locality, locality)

  return(gp)
}

#' HCPCS Level II Codes
#'
#' @template args-hcpcs
#'
#' @param limit `<lgl>` limit columns returned, default is `TRUE`
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
search_hcpcs <- function(hcpcs = NULL,
                         limit = TRUE,
                         ...) {

  lv2 <- pins::pin_read(mount_board(), "hcpcs")

  if (limit) {
    lv2 <- dplyr::select(
      lv2,
      hcpcs,
      description_short,
      description_long,
      price,
      multi_price,
      labcert,
      xref,
      tos,
      coverage,
      asc,
      betos)
  }

  lv2 <- search_if(lv2, lv2$hcpcs, hcpcs)

  return(lv2)
}

#' CPT (HCPCS Level I) Codes
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_cpt(c("39503", "43116", "33935", "11646"))
#'
#' @export
#'
#' @autoglobal
search_cpt <- function(hcpcs = NULL, ...) {

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors")

  cpt <- search_if(cpt, cpt$hcpcs, hcpcs)

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

  opp <- pins::pin_read(mount_board(), "opps")

  opp <- search_if(opp, opp$hcpcs, hcpcs)

  opp <- search_if(opp, opp$mac, mac)

  opp <- search_if(opp, opp$locality, locality)

  return(opp)
}
