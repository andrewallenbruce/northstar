#' Search Restructured BETOS Classifications
#'
#' [search_rbcs()] allows the user to group HCPCS codes into clinically
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
#' @param hcpcs < *chr* > vector of 5-digit HCPCS codes
#' @param category < *chr* > vector of RBCS categories:
#' + `Procedure` (n = 6920)
#' + `Test` (n = 3015)
#' + `DME` (n = 2971)
#' + `Treatment` (n = 1795)
#' + `Imaging` (n = 1097)
#' + `E&M` (n = 695)
#' + `Anesthesia` (n = 307)
#' + `Other` (n = 233)
#' @param subcategory < *chr* > vector of RBCS subcategories (53 unique in total)
#' @param family < *chr* > vector of RBCS families (178 unique in total)
#' @param procedure < *chr* > Procedure Type:
#' + `Major` (n = 3676)
#' + `Non-Procedure` (n = 10113)
#' + `Other` (n = 3244)
#' @param concatenate < *lgl* > Concatenate output, default is `TRUE`
#' @param ... Empty
#'
#' @return A [tibble][tibble::tibble-package] with the columns:
#'
#' |**Column**          |**Description**                              |
#' |:-------------------|:--------------------------------------------|
#' |`hcpcs`             |HCPCS or CPT code                            |
#' |`rbcs`              |RBCS Identifier                              |
#' |`rbcs_category`     |RBCS Category                                |
#' |`rbcs_subcategory`  |RBCS Subcategory                             |
#' |`rbcs_family`       |RBCS Family                                  |
#' |`rbcs_procedure`    |RBCS Major Procedure Indicator               |
#' |`date_hcpcs_add`    |Date HCPCS Code was added                    |
#' |`date_hcpcs_end`    |Date HCPCS Code was no longer effective      |
#' |`date_rbcs_assign`  |Earliest Date that the RBCS ID was effective |
#'
#' @examples
#' search_rbcs(hcpcs = c("J9264", "39503", "43116", "70170", "0001U"))
#' @export
#' @autoglobal
search_rbcs <- function(hcpcs       = NULL,
                        category    = NULL,
                        subcategory = NULL,
                        family      = NULL,
                        procedure   = NULL,
                        concatenate = TRUE,
                        ...) {

  rb <- pins::pin_read(mount_board(), "rbcs") |>
    dplyr::rename(procedure = major) |>
    dplyr::mutate(family = dplyr::if_else(
      family == "No RBCS Family", NA_character_, family))

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

  if (concatenate) {
    rb <- rb |>
      tidyr::unite("rbcs_category",
                   c(procedure, category),
                   sep = " ") |>
      tidyr::unite("rbcs_family",
                   c(subcategory, family),
                   sep = ": ",
                   na.rm = TRUE) |>
      dplyr::select(
        hcpcs,
        rbcs_category,
        rbcs_family
      )
  }
  return(rb)
}
