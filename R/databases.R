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

  if (!is.null(hcpcs)) {rv <- search_in(rv, rv$hcpcs, hcpcs)}

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

  if (!is.null(hcpcs)) {pmt <- search_in(pmt, pmt$hcpcs, hcpcs)}

  if (!is.null(mac)) {pmt <- search_in(pmt, pmt$mac, mac)}

  if (!is.null(locality)) {pmt <- search_in(pmt, pmt$locality, locality)}

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

  if (!is.null(state)) {gp <- search_in(gp, gp$state, state)}

  if (!is.null(mac)) {gp <- search_in(gp, gp$mac, mac)}

  if (!is.null(locality)) {gp <- search_in(gp, gp$locality, locality)}

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
#' search_hcpcs(c("A0021", "V5362", "J9264", "G8916"))
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

  if (!is.null(hcpcs)) {lv2 <- search_in(lv2, lv2$hcpcs, hcpcs)}

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

  if (!is.null(hcpcs)) {cpt <- search_in(cpt, cpt$hcpcs, hcpcs)}

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

  if (!is.null(hcpcs)) {opp <- search_in(opp, opp$hcpcs, hcpcs)}

  if (!is.null(mac)) {opp <- search_in(opp, opp$mac, mac)}

  if (!is.null(locality)) {opp <- search_in(opp, opp$locality, locality)}

  return(opp)
}
