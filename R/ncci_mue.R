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
#'    * [NCCI Medically Unlikely Edits](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits)
#'
#' @template args-hcpcs
#'
#' @param service_type `<chr>` `Practitioner`, `Outpatient Hospital`, or `DME Supplier`
#'
#' @param mai `<int>` MUE adjudication indicator; `1`, `2`, or `3`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_mue_edits(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' @autoglobal
#'
#' @export
get_mue_edits <- function(hcpcs        = NULL,
                          mai          = NULL,
                          service_type = NULL,
                          ...) {

  mue <- get_pin("mues")
  mue <- fuimus::search_in_if(mue, mue$hcpcs, hcpcs)
  mue <- fuimus::search_in_if(mue, mue$mue_service_type, service_type)
  mue <- fuimus::search_in_if(mue, mue$mue_mai, mai)
  return(mue)
}
