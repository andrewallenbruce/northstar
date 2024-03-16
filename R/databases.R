#' 2024 National Physician Fee Schedule Relative Value File
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' rvu(hcpcs = "11646")
#' @autoglobal
#' @export
rvu <- function(hcpcs = NULL) {

  rv <- pins::pin_read(mount_board(), "rvu")

  if (!is.null(hcpcs)) {
    hcpcs <- unique(hcpcs)
    rv    <- vctrs::vec_slice(rv, vctrs::vec_in(rv$hcpcs, hcpcs))
  }
  return(rv)
}

#' 2024 Physician Fee Schedule Payment Amount File
#' @param hcpcs description
#' @param mac description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' pfs(hcpcs    = c("39503", "43116", "33935", "11646"),
#'     locality = "01",
#'     mac      = "10212")
#' @autoglobal
#' @export
pfs <- function(hcpcs    = NULL,
                mac      = NULL,
                locality = NULL) {

  pmt <- pins::pin_read(mount_board(), "pymt")

  if (!is.null(hcpcs))    {
    hcpcs <- unique(hcpcs)
    pmt   <- vctrs::vec_slice(pmt, vctrs::vec_in(pmt$hcpcs, hcpcs))
  }

  if (!is.null(mac))      {
    mac <- unique(mac)
    pmt <- vctrs::vec_slice(pmt, vctrs::vec_in(pmt$mac, mac))
  }

  if (!is.null(locality)) {
    loc <- unique(locality)
    pmt <- vctrs::vec_slice(pmt, vctrs::vec_in(pmt$locality, loc))
  }
  return(pmt)
}

#' 2024 Geographic Practice Cost Indices
#' @param mac description
#' @param state description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' gpci(state = "GA", locality = "01", mac = "10212")
#' @export
#' @autoglobal
gpci <- function(mac      = NULL,
                 state    = NULL,
                 locality = NULL) {

  gp <- pins::pin_read(mount_board(), "gpci")
  gp$state <- as.character(gp$state)

  if (!is.null(mac))      {gp <- vctrs::vec_slice(gp, vctrs::vec_in(gp$mac, mac))}
  if (!is.null(state))    {gp <- vctrs::vec_slice(gp, vctrs::vec_in(gp$state, state))}
  if (!is.null(locality)) {gp <- vctrs::vec_slice(gp, vctrs::vec_in(gp$locality, locality))}

  return(gp)
}

#' 2024 Healthcare Common Procedure Coding System (HCPCS)
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' level2(hcpcs = c("39503", "43116", "33935", "11646"))
#' @export
#' @autoglobal
level2 <- function(hcpcs = NULL) {

  l2 <- pins::pin_read(mount_board(), "hcpcs")

  if (!is.null(hcpcs))    {
    hcpcs <- unique(hcpcs)
    l2    <- vctrs::vec_slice(l2, vctrs::vec_in(l2$hcpcs, hcpcs))
  }
  return(l2)
}

#' 2023 CPT Descriptors (Clinician & Consumer-Friendly)
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' descriptors(hcpcs = c("39503", "43116", "33935", "11646"))
#' @export
#' @autoglobal
descriptors <- function(hcpcs = NULL) {

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors")

  if (!is.null(hcpcs))    {
    hcpcs <- unique(hcpcs)
    cpt   <- vctrs::vec_slice(cpt, vctrs::vec_in(cpt$cpt, hcpcs))
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
#' @param hcpcs < *character* > HCPCS or CPT code
#' @param rbcs < *character* > RBCS ID
#' @param category < *character* > RBCS Category Description, e.g.
#' + Anesthesia
#' + DME
#' + E&M
#' + Imaging
#' + Other
#' + Procedure
#' + Test
#' + Treatment
#' @param subcategory < *character* > RBCS Subcategory Description
#' @param family < *character* > RBCS Family Description
#' @param major < *character* > Whether the HCPCS code is a `"Major"`,
#' `"Other"`, or `"Non-Procedure"` code.
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
#' |`major`             |RBCS Major Procedure Indicator               |
#' |`date_hcpcs_add`    |Date HCPCS Code was added                    |
#' |`date_hcpcs_end`    |Date HCPCS Code was no longer effective      |
#' |`date_rbcs_assign`  |Earliest Date that the RBCS ID was effective |
#'
#' @examplesIf interactive()
#' rbcs(hcpcs = "11646")
#' @export
#' @autoglobal
rbcs <- function(hcpcs       = NULL,
                 rbcs        = NULL,
                 category    = NULL,
                 subcategory = NULL,
                 family      = NULL,
                 major       = NULL) {

  rb <- pins::pin_read(mount_board(), "rbcs")

  if (!is.null(hcpcs))       {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$hcpcs, hcpcs))}
  if (!is.null(rbcs))        {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$rbcs, rbcs))}
  if (!is.null(category))    {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$category, category))}
  if (!is.null(subcategory)) {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$subcategory, subcategory))}
  if (!is.null(family))      {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$family, family))}
  if (!is.null(major))       {rb <- vctrs::vec_slice(rb, vctrs::vec_in(rb$major, major))}

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
#' @examplesIf interactive()
#' opps(hcpcs = "70170")
#' @export
#' @autoglobal
opps <- function(hcpcs = NULL,
                 mac = NULL,
                 locality = NULL) {

  op <- pins::pin_read(mount_board(), "opps")

  if (!is.null(hcpcs)) {
    hcpcs <- unique(hcpcs)
    op    <- vctrs::vec_slice(op, vctrs::vec_in(op$hcpcs, hcpcs))
  }

  if (!is.null(mac))      {
    mac <- unique(mac)
    op <- vctrs::vec_slice(op, vctrs::vec_in(op$mac, mac))
  }

  if (!is.null(locality)) {
    loc <- unique(locality)
    op <- vctrs::vec_slice(op, vctrs::vec_in(op$locality, loc))
  }
  return(op)
}

#' NCD Download Database
#'
#' Last Updated 2022-12-08
#'
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' ncd()
#' @export
#' @autoglobal
ncd <- function() {
  pins::pin_read(mount_board(), "ncd")
}
