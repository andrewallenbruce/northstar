#' NCCI Edits: Add-On Codes
#'
#' An Add-on Code (AOC) is a Healthcare Common Procedure Coding System (HCPCS) /
#' Current Procedural Terminology (CPT) code that describes a service that is
#' performed in conjunction with the primary service by the same practitioner.
#' An AOC is rarely eligible for payment if it’s the only procedure reported by
#' a practitioner.
#'
#' Add-on codes may be identified in three ways:
#'    1. The add-on code is in the AOC file as a Type 1, Type 2, or Type 3 AOC (formerly displayed as Type I, Type II or Type III).
#'    2. In the Medicare Physician Fee Schedule, an AOC generally has a global surgery period of `ZZZ`.
#'    3. In the CPT Manual, an add-on code is designated by the symbol `+`. The code descriptor of an AOC generally includes phrases such as "each additional" or "(List separately in addition to primary procedure)."
#'
#' Some CPT codes are identified as Add-on Codes (AOCs), which describe a
#' service that can only be reported in addition to a primary procedure.
#'
#' For specific primary codes, AOCs shall not be reported as a supplemental
#' service for other HCPCS/CPT codes not listed as a primary code.
#'
#' AOCs permit the reporting of significant supplemental services commonly
#' performed in addition to the primary procedure. By contrast, incidental
#' services that are necessary to accomplish the primary procedure (e.g., lysis
#' of adhesions in the course of an open cholecystectomy) are not separately
#' reportable with an AOC.
#'
#' Similarly, complications inherent in an invasive procedure occurring during
#' the procedure are not separately reportable. For example, control of bleeding
#' during an invasive procedure is considered part of the procedure and is not
#' separately reportable.
#'
#' Although the AOC and primary code are normally reported for the same date of
#' service, there are unusual circumstances where the two services may be
#' reported for different dates of service (e.g., 99291 and 99292).
#'
#' ## AOC Edit Types
#'
#' CMS divided the AOCs into three types to distinguish the payment policy for each type:
#'
#'    * Type 1: A Type 1 AOC has a limited number of identifiable primary procedure codes. The Change Request (CR) lists the Type 1 AOCs with their acceptable primary procedure codes. A Type 1 AOC, with one exception, is eligible for payment if one of the listed primary procedure codes is also eligible for payment to the same practitioner for the same patient on the same date of service. Claims processing contractors must adopt edits to assure that Type 1 AOCs are never paid unless a listed primary procedure code is also paid.
#'    * Type 2: A Type 2 AOC does not have a specific list of primary procedure codes. The CR lists the Type 2 AOCs without any primary procedure codes. Claims processing contractors are encouraged to develop their own lists of primary procedure codes for this type of AOC. Like the Type 1 AOCs, a Type 2 AOC is eligible for payment if an acceptable primary procedure code as determined by the claims processing contractor is also eligible for payment to the same practitioner for the same patient on the same date of service.
#'    * Type 3: A Type 3 AOC has some, but not all, specific primary procedure codes identified in the CPT Manual. The CR lists the Type 3 AOCs with the primary procedure codes that are specifically identifiable. However, claims processing contractors are advised that these lists are not exclusive and there are other acceptable primary procedure codes for AOCs in this Type. Claims processing contractors are encouraged to develop their own lists of additional primary procedure codes for this group of AOCs. Like the Type 1 AOCs, a Type 3 AOC is eligible for payment if an acceptable primary procedure code as determined by the claims processing contractor is also eligible for payment to the same practitioner for the same patient on the same date of service.
#'
#' ## PTP Edits
#'
#' In general, NCCI PTP edits do not include edits with most AOCs because edits
#' related to the primary procedure(s) are adequate to prevent inappropriate
#' payment for an add-on coded procedure (i.e., if an edit prevents payment of
#' the primary procedure code, the AOC shall not be paid).
#'
#' @note Version: 2024-04-01
#'
#'    * [NCCI Add-On Code Edits](https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits)
#'
#' @template args-hcpcs
#'
#' @param aoc_type `<chr>` AOC type; `Add-On` or `Primary`
#'
#' @param aoc_edit `<int>` AOC edit type:
#'    * `1`: Only Paid if Primary is Paid. Payment Eligible if Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'    * `2`: Some Specific Primaries. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'    * `3`: No Specific Primary Codes. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.
#'
#' @param unnest `<lgl>` Unnest the `aoc_complements` column, default is `FALSE`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_aocs(hcpcs_code = "22532", aoc_type = "Primary")
#'
#' search_aocs(hcpcs_code = c("22630", "77001", "88334", "64727"))
#'
#' search_aocs(hcpcs_code = "33935", unnest = TRUE)
#'
#' @autoglobal
#'
#' @family NCCI Edits
#'
#' @export
search_aocs <- function(hcpcs_code = NULL,
                        aoc_type = NULL,
                        aoc_edit = NULL,
                        unnest = FALSE,
                        ...) {

  aoc <- get_pin("ncci_aoc_nested")

  aoc <- fuimus::search_in_if(
    aoc,
    aoc$hcpcs_code,
    hcpcs_code)

  aoc  <- fuimus::search_in_if(
    aoc,
    aoc$aoc_edit,
    aoc_edit)

  aoc <- fuimus::search_in_if(
    aoc,
    aoc$aoc_type,
    aoc_type)

  aoc <- if (unnest) tidyr::unnest(aoc, aoc_complements) else aoc

  return(.add_class(aoc))
}

#' NCCI Edits: Medically Unlikely Edits (MUEs)
#'
#' National Correct Coding Initiative (NCCI) Medically Unlikely Edits (MUEs) are
#' used by Medicare Administrative Contractors (MACs) to reduce improper
#' payments for Part B claims.
#'
#' An MUE describes the maximum **Units of Service** (UOS) reported for a
#' HCPCS code on the vast majority of appropriately reported claims by the
#' same provider/supplier for the same beneficiary on the same date of service.
#'
#' Not all HCPCS codes have an MUE. Although CMS publishes most MUE values,
#' other MUE values are confidential. Confidential MUE values are not
#' releasable. The confidential status of MUEs is subject to change.
#'
#' ## MUE Edit Types
#'
#' MUEs are adjudicated as either a **Claim Line** edit or **Date of Service**
#' edit, the latter of which has two subclasses: *Policy* and *Clinical*.
#'
#' If the MUE is adjudicated as a Claim Line edit, the HCPCS code's UOS on each
#' claim line are compared to that code's MUE value. If the UOS exceeds the MUE
#' value, all UOS on that claim line are denied.
#'
#' DOS MUEs are used for HCPCS codes where it would be extremely unlikely that
#' more UOS than the MUE value would ever be performed on the same date of
#' service for the same patient.
#'
#' If the MUE is adjudicated as a DOS MUE, the sum of all UOS on each claim line
#' for the same date of service for the same HCPCS code is compared to the MUE
#' value. If the summed UOS exceeds the MUE value, all UOS for the HCPCS code
#' for that date of service are denied.
#'
#' ## MUE Adjudication Indicator
#'
#' An MAI of 1 indicates that the edit is a claim line MUE. An MAI of 2 or 3
#' indicates that the edit is a DOS MUE.
#'
#' If a HCPCS code has an MUE that is adjudicated as a claim line edit,
#' appropriate use of modifiers (i.e., 59 or XE, XP, XS, XU; 76, 77, 91,
#' anatomic) may be used to report the same HCPCS code on separate lines
#' of a claim.
#'
#' MUEs for HCPCS codes with an MAI of 2 are **absolute** date of service edits.
#' These are *"per day edits based on **policy**."* They obtain this MAI
#' designation because UOS on the same date of service (DOS) in excess of the
#' MUE value would contradict statute, regulation, or subregulatory guidance.
#'
#' MUEs with an MAI of 3 are *"per day edits based on **clinical** benchmarks."*
#' These are based on criteria (e.g., nature of service, prescribing
#' information) and historical data such that it would be possible but medically
#' highly unlikely that higher values would represent correctly reported
#' medically necessary services.
#'
#' ## Quarterly Updates
#'
#' CMS posts changes to each NCCI MUE edit file on a *quarterly* basis. This
#' includes additions, deletions, and revisions to published MUEs for
#' Practitioner Services, Outpatient Hospital Services, and DME Supplier
#' Services.
#'
#'    * [NCCI Medically Unlikely Edits](https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits)
#'
#' @template args-hcpcs
#'
#' @param mue_service `<chr>` `Practitioner`, `Outpatient`, or `DME`; default is
#'   `Practitioner`
#'
#' @param mue_mai `<int>` MUE adjudication indicator; `1`, `2`, or `3`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_mues(hcpcs_code = c("39503", "43116", "33935", "11646"))
#'
#' @autoglobal
#'
#' @family NCCI Edits
#'
#' @export
search_mues <- function(hcpcs_code = NULL,
                        mue_service = c("Practitioner", "Outpatient", "DME"),
                        mue_mai = NULL,
                        ...) {

  mue_service <- match.arg(mue_service)

  mue <- switch(
    mue_service,
    Practitioner = get_pin("ncci_mue_prac"),
    Outpatient   = get_pin("ncci_mue_out"),
    DME          = get_pin("ncci_mue_dme"))

  mue <- fuimus::search_in_if(mue, mue$hcpcs_code, hcpcs_code)

  mue <- fuimus::search_in_if(mue, mue$mue_mai, mue_mai)

  return(.add_class(mue))
}

#' NCCI Edits: Procedure to Procedure (PTP)
#'
#' National Correct Coding Initiative (NCCI) Procedure-to-Procedure (PTP) edits
#' prevent inappropriate payment of services that should not be reported
#' together. Each edit has a Column One (Comprehensive) and Column Two
#' (Component) HCPCS code.
#'
#' If a provider reports the two codes of an edit pair for the same beneficiary
#' on the same date of service, the Column One code is eligible for payment, but
#' the Column Two code is denied unless a clinically appropriate NCCI
#' PTP-associated modifier is also reported.
#'
#' Per National Correct Coding Initiative (NCCI) rules, Column 2 codes will
#' bundle their payment to specific Column 1 codes.
#'
#'    * When a Column 2 code is billed and paid before the Column 1 code is submitted, the Column 1 procedure code's allowed amount may be reduced by the already paid services.
#'    * Providers would need to add distinguishing modifiers to the Column 2 code before the Column 1 code can pay in full.
#'    * Reopenings or appeals should be used to add the modifiers.
#'
#' ## PTP Modifiers
#'
#' Modifiers that may be used under appropriate clinical circumstances to bypass
#' an NCCI PTP edit include:
#'
#'    * Anatomic: E1-E4, FA, F1-F9, TA, T1-T9, LT, RT, LC, LD, RC, LM, RI
#'    * Global Surgery: 24, 25, 57, 58, 78, 79
#'    * Other: 27, 59, 91, XE, XS, XP, XU
#'
#' It’s very important that NCCI PTP-associated modifiers only be used when
#' appropriate. In general, these circumstances relate to separate patient
#' encounters, separate anatomic sites, or separate specimens.
#'
#' Most edits involving paired organs or structures (e.g., eyes, ears,
#' extremities, lungs, kidneys) have NCCI PTP modifier indicators of `1` because
#' the two codes of the code-pair edit may be reported if performed on the
#' **contralateral** organs or structures.
#'
#' Most of these code-pairs should not be reported with NCCI PTP-associated
#' modifiers when performed on the **ipsilateral** organ or structure unless
#' there is a specific coding rationale to bypass the edit.
#'
#' The presence of an NCCI PTP edit indicates that the two codes generally can’t
#' be reported together unless the two corresponding procedures are performed at
#' two separate patient encounters or two separate anatomic locations.
#'
#' Similarly, if the two corresponding procedures are performed at the same
#' patient encounter and in contiguous structures in the same organ or anatomic
#' region, NCCI PTP-associated modifiers generally shouldn’t be used.
#'
#' Some Column One/Column Two correct coding edits would never warrant the use
#' of any of the modifiers associated with the NCCI PTP edits. These code pairs
#' are assigned a Correct Coding Modifier Indicator (CCMI) of `0`.
#'
#' NCCI PTP modifier indicators of `9` indicate that the use of NCCI
#' PTP-associated modifiers is not specified. This indicator is used for all
#' code pairs that have a deletion date that is the same as the effective date.
#' This indicator prevents blank spaces from appearing in the indicator field.
#'
#'
#' Modifier 59 may be used only if no other appropriate modifier describes the
#' service. Claim line edits allow use of NCCI PTP-associated **Modifier 91** to
#' bypass them if one or more of the individual laboratory tests are repeated on
#' the same date of service.
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
#' @param ptp_service `<chr>` `Practitioner` or `Outpatient`; default is
#'   `Practitioner`
#'
#' @param ptp_type `<chr>` `Comprehensive` (Column One) or `Component` (Column
#'   Two)
#'
#' @param ptp_mod `<int>`
#'    * `1`: Allowed: The code-pair is allowed with an NCCI PTP-associated modifier.
#'    * `0`: Not Allowed: An NCCI PTP-associated modifier is not allowed and will not bypass the edit.
#'    * `9`: Not Applicable: The use of NCCI PTP-associated modifiers is not specified.
#'
#' @param unnest `<lgl>` Unnest the `ptp_complements` column, default is `FALSE`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_ptps(hcpcs_code = c("39503", "43116", "33935", "11646"))
#'
#' search_ptps(hcpcs_code = "43116",
#'             ptp_type = "Component",
#'             ptp_mod = 0)
#'
#' @autoglobal
#'
#' @family NCCI Edits
#'
#' @export
search_ptps <- function(hcpcs_code = NULL,
                        ptp_service = c("Practitioner", "Outpatient"),
                        ptp_type = NULL,
                        ptp_mod = NULL,
                        unnest = FALSE,
                        ...) {

  ptp_service <- match.arg(ptp_service, c("Practitioner", "Outpatient"))

  ptp <- switch(
    ptp_service,
    Practitioner = get_pin("ncci_ptp_prac"),
    Outpatient   = get_pin("ncci_ptp_out"))

  ptp <- fuimus::search_in_if(ptp, ptp$hcpcs_code, hcpcs_code)

  ptp <- fuimus::search_in_if(ptp, ptp$ptp_type, ptp_type)

  ptp <- fuimus::search_in_if(ptp, ptp$ptp_mod, ptp_mod)

  ptp <- if (unnest) tidyr::unnest(ptp, ptp_complements) else ptp

  return(.add_class(ptp))
}
