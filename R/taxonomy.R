#' NUCC Provider Taxonomy Code Set
#'
#' `taxonomy_codes()` returns a [tibble()] of the current Health Care Provider
#' Taxonomy code set
#'
#' __Update Frequency__: _Biannually_
#'
#' @section Taxonomy Codes:
#' The Health Care Provider Taxonomy code set is a collection of unique
#' alphanumeric codes, ten characters in length. They contain no embedded logic.
#'
#' The code set Levels are organized to allow for drilling down to the
#' provider's most specific level of specialization, with three distinct "Levels":
#'
#' @section __Level I__ _Provider Grouping_:
#' A major grouping of service(s) or occupation(s) of health care providers.
#'
#' _Examples:_ Allopathic & Osteopathic Physicians, Dental Providers, Hospitals
#'
#' @section __Level II__ _Classification_:
#' A more specific service or occupation related to the Provider Grouping.
#'
#' For example, the classification for Allopathic & Osteopathic Physicians is
#' based upon the General Specialty Certificates as issued by the appropriate
#' national boards.
#'
#' The following boards will, however, have their general certificates appear as
#' Level III Areas of specialization strictly due to display limitations of the
#' code set for Boards that have multiple general certificates:
#'
#' _Medical Genetics, Preventive Medicine, Psychiatry & Neurology, Radiology,_
#' _Surgery, Otolaryngology, Pathology_
#'
#' @section __Level III__ _Area of Specialization_:
#' A more specialized area of the Classification in which a provider chooses to
#' practice or make services available.
#'
#' For example, the area of specialization for Allopathic & Osteopathic
#' Physicians is based upon the Subspecialty Certificates as issued by the
#' appropriate national boards.
#'
#' @section __Categories__ _(Level 0)_:
#' The code set includes three specialty categories:
#'
#' __Group (of Individuals)__:
#' 1. Multi-Specialty
#' 1. Single Specialty
#'
#' __Individuals__:
#' 1. Allopathic & Osteopathic Physicians
#' 1. Behavioral Health and Social Service Providers
#' 1. Chiropractic Providers
#' 1. Dental Providers
#' 1. Dietary and Nutritional Service Providers
#' 1. Emergency Medical Service Providers
#' 1. Eye and Vision Service Providers
#' 1. Nursing Service Providers
#' 1. Nursing Service Related Providers
#' 1. Other Service Providers
#' 1. Pharmacy Service Providers
#' 1. Physician Assistants and Advanced Practice Nursing Providers
#' 1. Podiatric Medicine and Surgery Service Providers
#' 1. Respiratory, Developmental, Rehabilitative and Restorative Service Providers
#' 1. Speech, Language and Hearing Service Providers
#' 1. Student, Health Care
#' 1. Technologists, Technicians, and Other Technical Service Providers
#'
#' __Non-Individuals__:
#' 1. Agencies
#' 1. Ambulatory Health Care Facilities
#' 1. Hospital Units
#' 1. Hospitals
#' 1. Laboratories
#' 1. Managed Care Organizations
#' 1. Nursing and Custodial Care Facilities
#' 1. Other Service Providers
#' 1. Residential Treatment Facilities
#' 1. Respite Care Facilities
#' 1. Suppliers
#' 1. Transportation Services
#'
#' @section Display Name:
#' The display name is a combination of the code name and the Level in
#' which the code is nested, which more precisely identifies the code.
#'
#' For example, Addiction Medicine is a physician subspecialty in Anesthesiology,
#' Family Medicine, Internal Medicine, Preventive Medicine, and Psychiatry &
#' Neurology.
#'
#' "Addiction Medicine" does not identify the specialty of
#' the physician, but the display name of "Addiction Medicine (Internal
#' Medicine) Physician" clearly does.
#'
#' In another example, "Radiology" could be confused with several
#' taxonomies, but "Radiology Chiropractor" more accurately specifies the provider.
#'
#' @section Description:
#' + `code`: Provider Taxonomy Code
#' + `category`: Indicates whether Taxonomy is Individual or Non-Individual, i.e., a group taxonomy
#' + `grouping`: Level I, Provider Grouping
#' + `classification`: Level II, Classification
#' + `specialization`: Level III, Area of Specialization
#' + `display_name`: Consumer-friendly taxonomy name, made of the code name and the Level in which the code is nested.
#' + `definition`: Definition of Taxonomy
#' + `version`: Three digit version of the code set. The first two digits indicate the year and the third digit indicates either the first release of the year ("0") or the second release of the year ("1").
#' + `release_date`: Date the version of the code set was released
#'
#' @source
#' [National Uniform Claim Committee](https://www.nucc.org/index.php/code-sets-mainmenu-41/provider-taxonomy-mainmenu-40/csv-mainmenu-57)
#'
#' @param shape shape of the data frame returned, 'wide' or 'long'
#' @param code vector of taxonomy codes to filter
#' @param unnest unnest the hierarchy column, default is `FALSE`
#' @param ... Empty
#'
#' @return A [tibble][tibble::tibble-package] with the columns:
#'
#' @examples
#' taxonomy("wide", code = c("207K00000X", "193200000X"))
#'
#' taxonomy("long", code = "207K00000X")
#'
#' taxonomy("long", code = "207K00000X", unnest = TRUE)
#' @autoglobal
#' @export
taxonomy <- function(shape = c('wide', 'long'),
                     code = NULL,
                     unnest = FALSE,
                     ...) {

  shape <- match.arg(shape)

  if (shape == 'wide') {
    txn <- pins::pin_read(mount_board(), "taxonomy")
  }

  if (shape == 'long') {
    txn <- pins::pin_read(mount_board(), "taxlong")
  }

  if (!is.null(code)) {
    txn <- vctrs::vec_slice(txn,
           vctrs::vec_in(txn$code,
           collapse::funique(code)))
  }

  if (shape == 'long' && unnest == TRUE) {
    txn <- tidyr::unnest(txn, cols = c(hierarchy))
  }
  return(txn)
}
