#' 2024 National Physician Fee Schedule Relative Value File
#' @param hcpcs description
#' @return a [dplyr::tibble()]
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
#' @param hcpcs description
#' @param mac description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examples
#' pfs(hcpcs    = c("39503", "43116", "33935", "11646"),
#'     locality = "01",
#'     mac      = "10212") |>
#' dplyr::glimpse()
#' @autoglobal
#' @export
pfs <- function(hcpcs    = NULL,
                mac      = NULL,
                locality = NULL) {

  # TODO convert filter(opps == "9") rows -> opps_nf and opps_f to NA

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
#' @param mac description
#' @param state description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examples
#' gpci(state    = "GA",
#'      locality = "01",
#'      mac      = "10212") |>
#' dplyr::glimpse()
#' @export
#' @autoglobal
gpci <- function(mac      = NULL,
                 state    = NULL,
                 locality = NULL) {

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
#' @param hcpcs description
#' @param limit_cols description
#' @return a [dplyr::tibble()]
#' @examples
#' level2(c("A0021", "V5362", "J9264", "G8916")) |> dplyr::glimpse()
#' @export
#' @autoglobal
level2 <- function(hcpcs = NULL, limit_cols = TRUE) {

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
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examples
#' descriptors(c("39503", "43116", "33935", "11646"))
#' @export
#' @autoglobal
descriptors <- function(hcpcs = NULL) {

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors")

  if (!is.null(hcpcs)) {
    cpt <- vctrs::vec_slice(cpt,
           vctrs::vec_in(cpt$cpt,
           collapse::funique(hcpcs))) |>
      tidyr::nest(clinician_descriptors = clinician_descriptor)
  }
  return(cpt)
}

#' Restructured BETOS Classification for HCPCS
#'
#' @description
#'
#' [rbcs()] allows the user to group HCPCS codes into clinically
#' meaningful categories based on the original _Berenson-Eggers Type of Service_
#' (BETOS) classification.
#'
#' @section From BETOS to RBCS:
#'
#' The Restructured BETOS Classification System (RBCS) is a taxonomy that allows
#' researchers to group Medicare Part B healthcare service codes into clinically
#' meaningful categories and subcategories.
#'
#' Based on the original Berenson-Eggers Type of Service (BETOS) classification
#' created in the 1980s, it includes notable updates such as Part B non-physician
#' services and undergoes annual updates by a technical expert panel of
#' researchers and clinicians.
#'
#' The general framework for grouping service codes into the new RBCS taxonomy
#' largely follows the same structure of BETOS. Like BETOS, the RBCS groups
#' HCPCS codes into categories, subcategories, and families – with categories
#' as the most aggregate level and families as the more granular level.
#'
#' All Medicare Part B service codes, including non-physician services, are
#' assigned to a 6-character RBCS taxonomy code.
#'
#' @section Links:
#'
#' + [Restructured BETOS Classification System](https://data.cms.gov/provider-summary-by-type-of-service/provider-service-classifications/restructured-betos-classification-system)
#' + [RBCS Data Dictionary](https://data.cms.gov/resources/restructured-betos-classification-system-data-dictionary)
#'
#' @section Update Frequency: Annually
#'
#' @param hcpcs < *character* > HCPCS code
#' @param category < *character* > RBCS Category:
#' + `Procedure` (n = 6920)
#' + `Test` (n = 3015)
#' + `DME` (n = 2971)
#' + `Treatment` (n = 1795)
#' + `Imaging` (n = 1097)
#' + `E&M` (n = 695)
#' + `Anesthesia` (n = 307)
#' + `Other` (n = 233)
#' @param subcategory < *character* > RBCS Subcategory (53 unique in total)
#' @param family < *character* > RBCS Family (178 unique in total)
#' @param procedure < *character* > Procedure Type:
#' + `Major` (n = 3676)
#' + `Non-Procedure` (n = 10113)
#' + `Other` (n = 3244)
#' @param limit_cols < *logical* > Limit Columns
#'
#' @return A [tibble][tibble::tibble-package] with the columns:
#'
#' |**Column**          |**Description**                              |
#' |:-------------------|:--------------------------------------------|
#' |`hcpcs`             |HCPCS or CPT code                            |
#' |`rbcs`              |RBCS Identifier                              |
#' |`category`          |RBCS Category                                |
#' |`subcategory`       |RBCS Subcategory                             |
#' |`family`            |RBCS Family                                  |
#' |`procedure`         |RBCS Major Procedure Indicator               |
#' |`date_hcpcs_add`    |Date HCPCS Code was added                    |
#' |`date_hcpcs_end`    |Date HCPCS Code was no longer effective      |
#' |`date_rbcs_assign`  |Earliest Date that the RBCS ID was effective |
#'
#' @examples
#' rbcs(hcpcs = c("J9264", "39503", "43116", "33935", "11646")) |>
#' dplyr::glimpse()
#' @export
#' @autoglobal
rbcs <- function(hcpcs       = NULL,
                 category    = NULL,
                 subcategory = NULL,
                 family      = NULL,
                 procedure   = NULL,
                 limit_cols  = TRUE) {

  # TODO procedure = major

  rb <- pins::pin_read(mount_board(), "rbcs") |>
    dplyr::rename(procedure = major)

  if (limit_cols) {
    rb <- dplyr::select(rb,
          hcpcs,
          rbcs_category    = category,
          rbcs_subcategory = subcategory,
          rbcs_family      = family,
          rbcs_procedure   = procedure)
  }

  if (!is.null(hcpcs)) {

    rb <- vctrs::vec_slice(rb,
          vctrs::vec_in(rb$hcpcs,
          collapse::funique(hcpcs)))}

  if (!is.null(procedure)) {

    procedure <- rlang::arg_match(procedure,
                 c("Major", "Non-Procedure", "Other"))

    rb <- vctrs::vec_slice(rb,
          vctrs::vec_in(rb$procedure, procedure))

  }

  if (!is.null(category)) {

    category <- rlang::arg_match(category,
    c("Procedure", "Test", "DME", "Treatment",
      "Imaging", "E&M", "Anesthesia", "Other"))

    rb <- vctrs::vec_slice(rb,
          vctrs::vec_in(rb$category, category))

    }

  if (!is.null(subcategory)) {
    rb <- vctrs::vec_slice(rb,
          vctrs::vec_in(rb$subcategory, subcategory))
  }

  if (!is.null(family)) {
    rb <- vctrs::vec_slice(rb,
          vctrs::vec_in(rb$family, family))
    }
  return(rb)
}

#' OPPSCAP
#'
#' Contains the payment amounts after the application of the OPPS-based payment
#' caps, except for carrier priced codes. For carrier price codes, the field
#' only contains the OPPS-based payment caps. Carrier prices cannot exceed the
#' OPPS-based payment caps.
#'
#' @param hcpcs description
#' @param mac description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examples
#' opps(hcpcs    = c("70170", "71550", "0689T", "75898"),
#'      mac      = "01112",
#'      locality = "05")
#' @export
#' @autoglobal
opps <- function(hcpcs    = NULL,
                 mac      = NULL,
                 locality = NULL) {

  op <- pins::pin_read(mount_board(), "opps") |>
    dplyr::rename(status = procstat)

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

#' NCD Download Database
#'
#' Last Updated 2022-12-08
#'
#' @param coverage Coverage level; `"Full"`, `"Restricted"`, `"None"`, `"Unknown"`
#' @return a [dplyr::tibble()]
#' @examples
#' ncd(coverage = "Unknown")
#' @export
#' @autoglobal
ncd <- function(coverage = NULL) {

  ncd <- pins::pin_read(mount_board(), "ncd")

  if (!is.null(coverage)) {
    coverage <- rlang::arg_match(coverage, c("Full", "Restricted", "None", "Unknown"))
    ncd      <- vctrs::vec_slice(ncd, ncd$coverage == coverage)
    }
  return(ncd)
}

#' Claim Adjustment Reason Codes (CARCs) and Remittance Advice Remark Codes (RARCs)
#' @noRd
#' @autoglobal
carc_rarc <- function() {pins::pin_read(mount_board(), "rarc_carc")}
