#' NCCI Add-On Code Edits
#'
#' Medicare NCCI Add-On Code Edits
#' Version: 2024-04-01
#'
#' An AOC is a HCPCS/CPT code that describes a service that, with rare
#' exception, is performed in conjunction with another primary service by the
#' same practitioner. An AOC is rarely eligible for payment if it is the only
#' procedure reported by a practitioner.
#'
#' Some CPT codes are identified as Add-on Codes (AOCs), which describe a
#' service that can only be reported in addition to a primary procedure.
#'
#' For specific primary codes, AOCs shall not be reported as a supplemental
#' service for other HCPCS/CPT codes not listed as a primary code.
#'
#' AOCs permit the reporting of significant supplemental services commonly
#' performed in addition to the primary procedure. By contrast, incidental
#' services that are necessary to accomplish the primary procedure (e.g.,
#' lysis of adhesions in the course of an open cholecystectomy) are not
#' separately reportable with an AOC. Similarly, complications inherent in an
#' invasive procedure occurring during the procedure are not separately
#' reportable. For example, control of bleeding during an invasive procedure
#' is considered part of the procedure and is not separately reportable.
#'
#' In general, NCCI PTP edits do not include edits with most AOCs because edits
#' related to the primary procedure(s) are adequate to prevent inappropriate
#' payment for an add-on coded procedure (i.e., if an edit prevents payment of
#' the primary procedure code, the AOC shall not be paid).
#'
#' ## AOC Types
#' For Type 1 AOCs edits, the CPT Professional or HCPCS files define all
#' acceptable primary codes. MACs should not allow other primary codes with
#' Type 1 AOCs.
#'
#' For Type 2 AOCs edits, the CPT Professional and HCPCS files do not define
#' any primary codes. MACs should develop their own lists of acceptable primary
#' codes.
#'
#' For Type 3 AOCs edits, the CPT Professional or HCPCS files define some, but
#' not all, acceptable primary codes. MACs should allow the listed primary codes
#' for these AOCs but may develop their own lists of additional acceptable
#' primary codes.
#'
#' Although the AOC and primary code are normally reported for the same date
#' of service, there are unusual circumstances where the two services may be
#' reported for different dates of service (e.g., 99291 and 99292).
#'
#' [Add-Ons Link](https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits)
#'
#' @param addon `<chr>` vector of HCPCS codes
#' @param primary `<chr>` vector of HCPCS codes
#' @param type `<int>` AOC edit type; `1`, `2`, `3`
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_addons(primary = c("39503", "43116", "33935", "11646"))
#' @autoglobal
#' @export
search_addons <- function(addon   = NULL,
                          primary = NULL,
                          type    = NULL,
                          ...) {

  aoc <- pins::pin_read(mount_board(), "aoc")

  if (!is.null(addon)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$addon,
           collapse::funique(addon)))
  }

  if (!is.null(primary)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$primary,
           collapse::funique(primary)))
  }

  if (!is.null(type)) {

    aoc <- vctrs::vec_slice(aoc,
           vctrs::vec_in(aoc$type,
           collapse::funique(type)))
  }
  return(aoc)
}

#' NCCI Medically Unlikely Edits
#'
#' Medicare NCCI Medically Unlikely Edits (MUEs)
#'
#' National Correct Coding Initiative (NCCI) Medically Unlikely Edits (MUEs)
#' are used by the Medicare Administrative Contractors (MACs), to reduce
#' improper payments for Part B claims.
#'
#' An MUE is the maximum **Units of Service** (UOS) reported for a HCPCS/CPT code
#' on the vast majority of appropriately reported claims by the same
#' provider/supplier for the same beneficiary on the same date of service.
#'
#' Not all HCPCS/CPT codes have an MUE. Although CMS publishes most MUE values
#' on its website, other MUE values are confidential. Confidential MUE values
#' are not releasable. The confidential status of MUEs is subject to change.
#'
#' ## Quarterly Version Update Changes
#'
#' CMS posts changes to each of its NCCI MUE published edit files on a
#' quarterly basis. This includes additions, deletions, and revisions to
#' published MUEs for Practitioner Services, Outpatient Hospital Services,
#' and DME Supplier Services.
#'
#' CMS began introducing date of service (DOS) MUEs. Over time CMS will convert
#' many, but not all, MUEs to DOS MUEs. MUEs are adjudicated either as claim
#' line edits or DOS edits. If the MUE is adjudicated as a claim line edit,
#' the UOS on each claim line are compared to the MUE value for the HCPCS/CPT
#' code on that claim line. If the UOS exceed the MUE value, all UOS on that
#' claim line are denied.
#'
#' If the MUE is adjudicated as a DOS MUE, all UOS on each claim line for the
#' same date of service for the same HCPCS/CPT code are summed, and the sum is
#' compared to the MUE value. If the summed UOS exceed the MUE value, all UOS
#' for the HCPCS/CPT code for that date of service are denied. Denials due to
#' claim line MUEs or DOS MUEs may be appealed to the local claims processing
#' contractor. DOS MUEs are used for HCPCS/CPT codes where it would be
#' extremely unlikely that more UOS than the MUE value would ever be performed
#' on the same date of service for the same patient.
#'
#' ## MUE Adjudication Indicator (MAI)
#' An MAI of 1 indicates that the edit is a claim line MUE. An MAI of 2 or 3
#' indicates that the edit is a DOS MUE.
#'
#' If a HCPCS/CPT code has an MUE that is adjudicated as a claim line edit,
#' appropriate use of CPT modifiers (i.e., 59 or XE, XP, XS, XU; 76, 77, 91,
#' anatomic) may be used to report the same HCPCS/CPT code on separate lines
#' of a claim.
#'
#' MUEs for HCPCS codes with an MAI of 2 are absolute date of service edits.
#' These are "per day edits based on policy." These have been rigorously
#' reviewed and vetted within CMS and obtain this MAI designation because UOS
#' on the same date of service (DOS) in excess of the MUE value would be
#' considered impossible because it was contrary to statute, regulation, or
#' subregulatory guidance.
#'
#' MUEs for HCPCS codes with an MAI of 3 are "per day edits based on clinical
#' benchmarks." These are based on criteria (e.g., nature of service,
#' prescribing information) combined with data such that it would be possible
#' but medically highly unlikely that higher values would represent correctly
#' reported medically necessary services
#'
#' [MUE Link](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits)
#'
#' @param hcpcs `<chr>` vector of HCPCS codes
#' @param service `<chr>` service type; one of DME Supplier, Outpatient Hospital, Practitioner
#' @param mai `<int>` MUE adjudication indicator; `1`, `2`, `3`
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_mue(hcpcs = c("39503", "43116", "33935", "11646"))
#' @autoglobal
#' @export
search_mue <- function(hcpcs   = NULL,
                       service = NULL,
                       mai     = NULL,
                       ...) {

  mue <- pins::pin_read(mount_board(), "mues")

  if (!is.null(hcpcs)) {

    mue <- vctrs::vec_slice(mue,
           vctrs::vec_in(mue$hcpcs,
           collapse::funique(hcpcs)))
  }

  if (!is.null(service)) {

    mue <- vctrs::vec_slice(mue,
           vctrs::vec_in(mue$service_type,
           collapse::funique(service)))

    if (!is.null(mai)) {

      mue <- vctrs::vec_slice(mue,
             vctrs::vec_in(mue$mai,
             collapse::funique(mai)))
    }
  }
  return(mue)
}

#' Procedure to Procedure (PTP) Edits
#'
#' National Correct Coding Initiative (NCCI) Procedure-to-Procedure (PTP) edits
#' prevent inappropriate payment of services that should not be reported
#' together. Each edit has a Column One and Column Two HCPCS/CPT code.
#'
#' If a provider reports the two codes of an edit pair for the same beneficiary
#' on the same date of service, the Column One code is eligible for payment,
#' but the Column Two code is denied unless a clinically appropriate NCCI
#' PTP-associated modifier is also reported.
#'
#' [PTP Link](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits)
#'
#' @param column_1 `<chr>` Comprehensive HCPCS code
#' @param column_2 `<chr>` Component HCPCS code
#' @param mod `<int>` `1` = Allowed, `0` = Not Allowed, `9` = Not Applicable
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' search_ptp(column_1 = c("39503", "43116", "33935", "11646"))
#' @autoglobal
#' @export
search_ptp <- function(column_1 = NULL,
                       column_2 = NULL,
                       mod      = NULL,
                       ...) {

  ptp <- pins::pin_read(mount_board(), "ptp")

  if (!is.null(column_1)) {

    ptp <- vctrs::vec_slice(ptp,
           vctrs::vec_in(ptp$column_1,
           collapse::funique(column_1)))
  }

  if (!is.null(column_2)) {

    ptp <- vctrs::vec_slice(ptp,
           vctrs::vec_in(ptp$column_2,
           collapse::funique(column_2)))
  }

  if (!is.null(mod)) {

    ptp <- vctrs::vec_slice(ptp,
           vctrs::vec_in(ptp$modifier,
           collapse::funique(mod)))
  }
  return(ptp)
}
