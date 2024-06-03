#' Procedure to Procedure (PTP) Edits
#'
#' National Correct Coding Initiative (NCCI) Procedure-to-Procedure (PTP) edits
#' prevent inappropriate payment of services that should not be reported
#' together. Each edit has a Column One and Column Two HCPCS/CPT code.
#'
#' If a provider reports the two codes of an edit pair for the same beneficiary
#' on the same date of service, the Column One code is eligible for payment, but
#' the Column Two code is denied unless a clinically appropriate NCCI
#' PTP-associated modifier is also reported.
#'
#' The NCCI [PTP edits](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits) and [MUEs](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits) are usually updated at least quarterly.
#'
#' ### NCCI Modifiers
#' Modifiers that may be used under appropriate clinical circumstances to bypass
#' an NCCI PTP edit include:
#'    * Anatomic: E1-E4, FA, F1-F9, TA, T1-T9, LT, RT, LC, LD, RC, LM, RI
#'    * Global Surgery: 24, 25, 57, 58, 78, 79
#'    * Other: 27, 59, 91, XE, XS, XP, XU
#'
#' It’s very important that NCCI PTP-associated modifiers only be used when
#' appropriate. In general, these circumstances relate to separate patient
#' encounters, separate anatomic sites, or separate specimens. (See subsequent
#' discussion of modifiers in this section.)
#'
#' Most edits involving paired organs or structures (e.g., eyes, ears,
#' extremities, lungs, kidneys) have NCCI PTP modifier indicators of `1` because
#' the two codes of the code pair edit may be reported if performed on the
#' contralateral organs or structures.
#'
#' Most of these code pairs should not be reported with NCCI PTP-associated
#' modifiers when performed on the ipsilateral organ or structure unless there
#' is a specific coding rationale to bypass the edit.
#'
#' The presence of an NCCI PTP edit indicates that the two codes generally can’t
#' be reported together unless the two corresponding procedures are performed at
#' two separate patient encounters or two separate anatomic locations.
#'
#' Similarly, if the two corresponding procedures are performed at the same
#' patient encounter and in contiguous structures in the same organ or anatomic
#' region, NCCI PTP-associated modifiers generally shouldn’t be used.
#'
#' Modifier 59 ([MLN 1783722](https://www.cms.gov/files/document/mln1783722-proper-use-modifiers-59-xe-xp-xs-and-xu.pdf))
#' may be used only if no other appropriate modifier describes the service. The
#' article provides more information on the appropriate use of the 59 modifier.
#'
#' Some Column One/Column Two correct coding edits would ever warrant the use of
#' any of the modifiers associated with the NCCI PTP edits. These code pairs are
#' assigned a correct coding modifier indicator (CCMI) of `0`.
#'
#' Claim line edits allow use of NCCI PTP-associated **Modifier 91** to bypass
#' them if one or more of the individual laboratory tests are repeated on the
#' same date of service.
#'
#' The repeat testing must be medically reasonable and necessary and can’t be
#' performed to "confirm initial results; due to testing problems with specimens
#' and equipment or for any other reason when a normal, one-time, reportable
#' result is all that is required."
#'
#'    * [Procedure to Procedure (PTP) Edits](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits)
#'
#' @template args-hcpcs
#'
#' @param ptp_type `<chr>` `comprehensive` (Column One code) or `component`
#'   (Column Two code)
#'
#' @param ptp_edit_mod `<int>`
#'    * `1`: Allowed
#'    * `0`: Not Allowed
#'    * `9`: Not Applicable
#'
#' @param current `<lgl>` return only current edits, default is `TRUE`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_ptp_edits(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' get_ptp_edits(hcpcs        = "43116",
#'               ptp_type     = "component",
#'               ptp_edit_mod = 0)
#'
#' @autoglobal
#'
#' @export
get_ptp_edits <- function(hcpcs        = NULL,
                          ptp_type     = NULL,
                          ptp_edit_mod = NULL,
                          current      = TRUE,
                          ...) {

  ptp <- get_pin("ptp_long")
  ptp <- fuimus::search_in_if(ptp, ptp$hcpcs, hcpcs)
  ptp <- fuimus::search_in_if(ptp, ptp$ptp_type, ptp_type)
  ptp <- fuimus::search_in_if(ptp, ptp$ptp_edit_mod, ptp_edit_mod)

  if(current) {
    ptp <- vctrs::vec_slice(
      ptp,
      ptp$ptp_deleted == as.Date("9999-12-31"))

    ptp <- dplyr::select(ptp, -ptp_deleted)

  }

  return(.add_class(ptp))
}
