#' 2024 National Physician Fee Schedule Relative Value File
#'
#' @param hcpcs 5-digit HCPCS code
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' rvu(c("A0021", "V5362", "J9264", "G8916")) |>
#' dplyr::glimpse()
#' @autoglobal
#' @export
rvu <- function(hcpcs = NULL) {

  rv <- pins::pin_read(mount_board(), "rvu")

  if (!is.null(hcpcs)) {

    rv <- vctrs::vec_slice(rv,
          vctrs::vec_in(rv$hcpcs,
          collapse::funique(hcpcs)))
  }
  return(rv)
}

#' 2024 Physician Fee Schedule Payment Amount File
#'
#' @param hcpcs 5-digit HCPCS code
#' @param mac 5-digit Medicare Administrative Contractor (MAC) code
#' @param locality 2-digit locality ID
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' pfs(hcpcs    = c("39503", "43116", "33935", "11646"),
#'     locality = "01",
#'     mac      = "10212") |>
#' dplyr::glimpse()
#' @autoglobal
#' @export
pfs <- function(hcpcs    = NULL,
                mac      = NULL,
                locality = NULL,
                ...) {

  # TODO convert filter(opps == "9")
  # rows -> opps_nf and opps_f to NA

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

#' 2024 Geographic Practice Cost Indices
#'
#' @param mac 5-digit Medicare Administrative Contractor (MAC) code
#' @param state 2-character State abbreviation
#' @param locality 2-digit locality ID
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' gpci(state    = "GA",
#'      locality = "01",
#'      mac      = "10212") |>
#' dplyr::glimpse()
#' @export
#' @autoglobal
gpci <- function(mac      = NULL,
                 state    = NULL,
                 locality = NULL,
                 ...) {

  # TODO convert state col to character

  gp <- pins::pin_read(mount_board(), "gpci")

  gp$state <- as.character(gp$state)

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

#' 2024 Healthcare Common Procedure Coding System (HCPCS)
#' @param hcpcs 5-digit Level II HCPCS code
#' @param limit_cols limit the number of columns returned
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' level2(c("A0021", "V5362", "J9264", "G8916")) |> dplyr::glimpse()
#' @export
#' @autoglobal
level2 <- function(hcpcs = NULL,
                   limit_cols = TRUE,
                   ...) {

  # TODO coverage = cov,
  # TODO asc = asc_grp,
  # TODO description = short_description

  l2 <- pins::pin_read(mount_board(), "hcpcs") |>
    dplyr::rename(description = short_description,
                  description_long = long_description,
                  asc = asc_grp,
                  coverage = cov,
                  mult = mult_pi)

  if (limit_cols) {
    l2 <- dplyr::select(l2,
          hcpcs,
          description,
          description_long,
          price,
          mult,
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

  l2 <- case_asc(l2, asc) |>
    case_coverage(coverage) |>
    case_pricing(price) |>
    case_multiple_pricing(mult) |>
    case_tos(tos)

  return(l2)
}

#' 2023 CPT Descriptors (Clinician & Consumer-Friendly)
#' @param hcpcs 5-digit Level I, Category I HCPCS code
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' descriptors(c("39503", "43116", "33935", "11646"))
#' @export
#' @autoglobal
descriptors <- function(hcpcs = NULL) {

  # TODO tidyr::nest(clinician_descriptors = clinician_descriptor)
  # TODO rename description_consumer = consumer_descriptor
  # TODO rename description_clinician = clinician_descriptor

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors") |>
    tidyr::nest(clinician_descriptors = clinician_descriptor)

  if (!is.null(hcpcs)) {
    cpt <- vctrs::vec_slice(cpt,
           vctrs::vec_in(cpt$cpt,
           collapse::funique(hcpcs)))
  }
  return(cpt)
}

#' OPPSCAP
#'
#' Contains the payment amounts after the application of the OPPS-based payment
#' caps, except for carrier priced codes. For carrier price codes, the field
#' only contains the OPPS-based payment caps. Carrier prices cannot exceed the
#' OPPS-based payment caps.
#'
#' @param hcpcs *<chr>* vector of 5-digit HCPCS codes
#' @param mac *<chr>* vector of 5-digit Medicare Administrative Contractor (MAC) codes
#' @param locality *<chr>* vector of 2-digit locality IDs
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' opps(hcpcs = "71550", mac = "01112", locality = "05")
#' @export
#' @autoglobal
opps <- function(hcpcs    = NULL,
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

#' Level I and II HCPCS Modifiers
#'
#' A modifier provides the means to report or indicate that a service or
#' procedure that has been performed has been altered by some specific
#' circumstance but not changed in its definition or code.
#'
#' Modifiers also enable health care professionals to effectively respond to
#' payment policy requirements established by other entities.
#'
#' @param mod *<chr>* 2-digit HCPCS modifier
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' modifiers(mod = c("25", "59"))
#' @export
#' @autoglobal
modifiers <- function(mod = NULL, ...) {

  md <- pins::pin_read(mount_board(), "modifiers")

  if (!is.null(mod)) {
    md <- vctrs::vec_slice(md,
          vctrs::vec_in(md$mod,
          collapse::funique(mod)))
  }
  return(md)
}
