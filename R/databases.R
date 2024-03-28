#' Relative Value File
#'
#' @param hcpcs `<chr>` 5-digit HCPCS code
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_rvu(c("A0021", "V5362", "J9264", "G8916"))
#' @autoglobal
#' @export
search_rvu <- function(hcpcs = NULL, ...) {

  rv <- pins::pin_read(mount_board(), "rvu")

  if (!is.null(hcpcs)) {

    rv <- vctrs::vec_slice(rv,
          vctrs::vec_in(rv$hcpcs,
          collapse::funique(hcpcs)))
  }
  return(rv)
}

#' Payment Amount File
#'
#' @param hcpcs `<chr>` 5-digit HCPCS code
#' @param mac `<chr>` 5-digit MAC ID code
#' @param locality `<chr>` 2-digit Locality ID
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_payment(hcpcs    = c("39503", "43116", "33935", "11646"),
#'                locality = "01",
#'                mac      = "10212")
#' @autoglobal
#' @export
search_payment <- function(hcpcs    = NULL,
                           mac      = NULL,
                           locality = NULL,
                           ...) {

  pmt <- pins::pin_read(mount_board(), "pymt")

  if (!is.null(hcpcs)) {
    pmt <- vctrs::vec_slice(pmt,
           vctrs::vec_in(pmt$hcpcs,
           collapse::funique(hcpcs)))
  }

  if (!is.null(mac)) {
    pmt <- vctrs::vec_slice(pmt,
           vctrs::vec_in(pmt$mac,
           collapse::funique(mac)))
  }

  if (!is.null(locality)) {
    pmt <- vctrs::vec_slice(pmt,
           vctrs::vec_in(pmt$locality,
           collapse::funique(locality)))
  }
  return(pmt)
}

#' Geographic Practice Cost Indices
#'
#' @param mac `<chr>` 5-digit MAC code
#' @param state `<chr>` 2-character State abbreviation
#' @param locality `<chr>` 2-digit Locality ID
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_gpci(state = "GA", locality = "01", mac = "10212")
#' @export
#' @autoglobal
search_gpci <- function(mac      = NULL,
                        state    = NULL,
                        locality = NULL,
                        ...) {

  gp <- pins::pin_read(mount_board(), "gpci") |>
    dplyr::rename(area = name)

  if (!is.null(state)) {
    gp <- vctrs::vec_slice(gp,
          vctrs::vec_in(gp$state,
          collapse::funique(state)))
  }

  if (!is.null(mac)) {
    gp <- vctrs::vec_slice(gp,
          vctrs::vec_in(gp$mac,
          collapse::funique(mac)))
  }

  if (!is.null(locality)) {
    gp <- vctrs::vec_slice(gp,
          vctrs::vec_in(gp$locality,
          collapse::funique(locality)))
  }
  return(gp)
}

#' HCPCS Level II Codes
#'
#' @param hcpcs `<chr>` 5-digit Level II HCPCS code
#' @param limit `<lgl>` limit columns returned, default is `TRUE`
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_hcpcs(c("A0021", "V5362", "J9264", "G8916"))
#' @export
#' @autoglobal
search_hcpcs <- function(hcpcs = NULL,
                         limit = TRUE,
                         ...) {

  l2 <- pins::pin_read(mount_board(), "hcpcs")

  if (limit) {
    l2 <- dplyr::select(l2,
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

  if (!is.null(hcpcs)) {
    l2 <- vctrs::vec_slice(l2,
          vctrs::vec_in(l2$hcpcs,
          collapse::funique(hcpcs)))
  }

  # l2 <- case_asc(l2, asc) |>
  #   case_coverage(coverage) |>
  #   case_pricing(price) |>
  #   case_multiple_pricing(mult) |>
  #   case_tos(tos)

  return(l2)
}

#' CPT (HCPCS Level I) Codes
#'
#' @param hcpcs `<chr>` 5-digit CPT codes
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_cpt(c("39503", "43116", "33935", "11646"))
#' @export
#' @autoglobal
search_cpt <- function(hcpcs = NULL, ...) {

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors")

  if (!is.null(hcpcs)) {
    cpt <- vctrs::vec_slice(cpt,
           vctrs::vec_in(cpt$hcpcs,
           collapse::funique(hcpcs)))
  }
  return(cpt)
}

#' Outpatient Prospective Payment System (OPPS) Capitations
#'
#' Contains the payment amounts after the application of the OPPS-based payment
#' caps, except for carrier priced codes. For carrier price codes, the field
#' only contains the OPPS-based payment caps. Carrier prices cannot exceed the
#' OPPS-based payment caps.
#'
#' @param hcpcs `<chr>` vector of 5-digit HCPCS codes
#' @param mac `<chr>` vector of 5-digit MAC codes
#' @param locality `<chr>` vector of 2-digit Locality IDs
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_opps(hcpcs = "71550", mac = "01112")
#' @export
#' @autoglobal
search_opps <- function(hcpcs    = NULL,
                        mac      = NULL,
                        locality = NULL,
                        ...) {

  op <- pins::pin_read(mount_board(), "opps")

  if (!is.null(hcpcs)) {
    op <- vctrs::vec_slice(op,
          vctrs::vec_in(op$hcpcs,
          collapse::funique(hcpcs)))
  }

  if (!is.null(mac)) {
    op <- vctrs::vec_slice(op,
          vctrs::vec_in(op$mac,
          collapse::funique(mac)))
  }

  if (!is.null(locality)) {
    op <- vctrs::vec_slice(op,
          vctrs::vec_in(op$locality,
          collapse::funique(locality)))
  }
  return(op)
}
