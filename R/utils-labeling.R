#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_section` column
#' @examples
#' x <- c("39503", "99215", "99140",
#'        "69990", "70010", "0222U",
#'        "V5299", "7010F", "0074T")
#'
#' dplyr::tibble(hcpcs = x) |>
#' case_section_cpt(hcpcs)
#' @export
#' @autoglobal
case_section_cpt <- function(df, col) {

  df |>
    dplyr::mutate(section = dplyr::case_match(
      {{ col }},
      as.character(c(99202:99499)) ~ "E&M [99202-99499]",
      as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology [00100-01999, 99100-99140]",
      as.character(c(10004:69990)) ~ "Surgery [10004-69990]",
      as.character(c(70010:79999)) ~ "Radiology [70010-79999]",
      as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Path & Lab [80047-89398, 0001U-0222U]",
      as.character(c(90281:99199, 99500:99607)) ~ "Medicine [90281-99199, 99500-99607]"),
      .after = {{ col }})
}

#' Add HCPCS Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `hcpcs_section` column
#' @examples
#' x <- c("39503", "99215", "99140",
#'        "69990", "70010", "0222U",
#'        "V5299", "7010F", "0074T")
#'
#' dplyr::tibble(hcpcs = x) |>
#' case_section_hcpcs(hcpcs)
#' @export
#' @autoglobal
case_section_hcpcs <- function(df, col) {

  df |>
    dplyr::mutate(section = dplyr::case_match(
      substr({{ col }}, 1, 1),
      "A" ~ "Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental",
      "B" ~ "Enteral and Parenteral Therapy",
      "C" ~ "Temporary Hospital Outpatient Prospective Payment System",
      "D" ~ "Dental Procedures",
      "E" ~ "Durable Medical Equipment",
      "G" ~ "Temporary Procedures & Professional Services",
      "H" ~ "Rehabilitative Services",
      "J" ~ "Drugs Administered Other Than Oral Method, Chemotherapy Drugs",
      "K" ~ "Temporary Codes for Durable Medical Equipment Regional Carriers",
      "L" ~ "Orthotic/Prosthetic Procedures",
      "M" ~ "Medical Services",
      "P" ~ "Pathology and Laboratory",
      "Q" ~ "Temporary Codes",
      "R" ~ "Diagnostic Radiology Services",
      "S" ~ "Private Payer Codes",
      "T" ~ "State Medicaid Agency Codes",
      "V" ~ "Vision/Hearing Services"
      ),
      .after = {{ col }})
}

#' Add HCPCS Level Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `hcpcs_level` column
#' @examples
#' x <- c("39503", "99215", "99140",
#'        "69990", "70010", "0222U",
#'        "V5299", "7010F", "0074T")
#'
#' dplyr::tibble(hcpcs = x) |> case_level(hcpcs)
#' @export
#' @autoglobal
case_level <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(level = dplyr::case_when(
      is_level_I({{ col }}) ~ "1",
      is_level_II({{ col }}) ~ "2"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}

#' Add CPT Category Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_category` column
#' @examples
#' x <- c("39503", "99215", "99140",
#'        "69990", "70010", "0222U",
#'        "V5299", "7010F", "0074T")
#'
#' dplyr::tibble(hcpcs = x) |> case_category(hcpcs)
#' @export
#' @autoglobal
case_category <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(category = dplyr::case_when(
      is_category_I({{ col }}) ~ "1",
      is_category_II({{ col }}) ~ "2",
      is_category_III({{ col }}) ~ "3"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}

#' Add Global Days Descriptions
#' @param df data frame
#' @param col column of Global Days indicators
#' @return A [tibble][tibble::tibble-package] with a `global_description` column
#' @examples
#' x <- c("000", "010", "090", "MMM",
#'        "XXX", "YYY", "ZZZ")
#'
#' dplyr::tibble(global = x) |> case_global(global)
#' @export
#' @autoglobal
case_global <- function(df, col) {

  df |>
    dplyr::mutate(global_description = dplyr::case_match(
      {{ col }},
      "000" ~ "Endoscopic or minor procedure with related Preoperative and Postoperative RVUs on the day of the procedure only included in the fee schedule payment amount. E&M services on the day of the procedure generally not payable.",
      "010" ~ "Minor procedure with Preoperative RVUs on the day of the procedure and Postoperative RVUs during a 10-day postoperative period included in the fee schedule amount. E&M services on the day of the procedure and during the 10-day postoperative period generally not payable.",
      "090" ~ "Major surgery with a 1-day Preoperative period and 90-day Postoperative period included in fee schedule amount.",
      "MMM" ~ "Maternity codes. Usual Global period does not apply.",
      "XXX" ~ NA_character_,
        # "Global concept does not apply.",
      "YYY" ~ "Carrier determines if Global concept applies and, if appropriate, establishes Postoperative period.",
      "ZZZ" ~ "Code related to another service and is always included in Global period of other service."
    ),
    .after = {{ col }})
}

#' Add Diagnostic Imaging Descriptions
#'
#' Identifies the applicable Diagnostic Service family for
#' HCPCS codes with a Multiple Procedure indicator of 4.
#'
#' @param df data frame
#' @param col column of Diagnostic Imaging indicators
#' @return A [tibble][tibble::tibble-package] with a `dximg_description` column
#' @examples
#' dplyr::tibble(dximg = stringr::str_pad(c(1:11, 88, 99),
#'               width = "2", pad = "0")) |>
#'               case_imaging(dximg)
#' @export
#' @autoglobal
case_imaging <- function(df, col) {

  df |>
    dplyr::mutate(dximg_description = dplyr::case_match(
      {{ col }},
      "01" ~ "Ultrasound (Chest / Abdomen / Pelvis-Non-Obstetrical)",
      "02" ~ "CT and CTA (Chest / Thorax / Abd / Pelvis)",
      "03" ~ "CT and CTA (Head / Brain / Orbit / Maxillofacial / Neck)",
      "04" ~ "MRI and MRA (Chest / Abd / Pelvis)",
      "05" ~ "MRI and MRA (Head / Brain / Neck)",
      "06" ~ "MRI and MRA (Spine)",
      "07" ~ "CT (Spine)",
      "08" ~ "MRI and MRA (Lower Extremities)",
      "09" ~ "CT and CTA (Lower Extremities)",
      "10" ~ "MRI and MRA (Upper Extremities and Joints)",
      "11" ~ "CT and CTA (Upper Extremities)",
      "88" ~ "Subject to Reduction of TC or PCDiagnostic Imaging",
      "99" ~ NA_character_
    ),
    .after = {{ col }})
}

#' Add Physician Supervision Descriptions
#'
#' This field is for use in post payment review.
#'
#' @param df data frame
#' @param col column of Physician Supervision indicators
#' @return A [tibble][tibble::tibble-package] with a `supvis_description` column
#' @examples
#' x <- stringr::str_pad(
#'      c(1:6, 9, 21, 22, 66, "6A", 77, "7A"),
#'      width = "2", pad = "0")
#'
#' dplyr::tibble(supvis = x) |> case_supervision(supvis)
#' @export
#' @keywords internal
#' @autoglobal
case_supervision <- function(df, col) {

  df |>
    dplyr::mutate(supvis_description = dplyr::case_match(
      {{ col }},
      "01" ~ "Must be performed under General supervision",
      "02" ~ "Must be performed under Direct supervision",
      "03" ~ "Must be performed under Personal supervision",
      "04" ~ "Must be performed under General supervision unless performed by Qualified, Independent Psychologist or Clinical Psychologist",
      "05" ~ "Must be performed under General supervision unless performed by Qualified Audiologist",
      "06" ~ "Must be performed by Physician or Qualified Electrophysiological Clinical Specialist (ABPTS-certified) permitted to provide procedure under State law",
      "21" ~ "May be performed by Certified Technician under General supervision; otherwise, Direct supervision",
      "22" ~ "May be performed by Technician with online real-time Physician contact",
      "66" ~ "May be performed by Physician or ABPTS-certified physical therapist with certification in procedure",
      "6A" ~ "In addition to Level 66 rule, ABPTS-certified PT may supervise another PT, but only ABPTS-certified PT may bill",
      "77" ~ "Must be performed by either ABPTS-certified PT, Uncertified PT under Direct supervision, or Certified Technician under General supervision",
      "7A" ~ "In addition to Level 77 rule, ABPTS-certified PT may supervise another PT, but only ABPTS-certified PT may bill",
      "09" ~ NA_character_
    ),
    .after = {{ col }})
}

#' Add Team Surgery Descriptions
#'
#' Modifier 66: Services for which Team Surgeons may be paid
#'
#' @param df data frame
#' @param col column of Team Surgery indicators
#' @return A [tibble][tibble::tibble-package] with a `team_description` column
#' @examples
#' dplyr::tibble(surg_team = c(0:2, "9")) |> case_team(surg_team)
#' @export
#' @keywords internal
#' @autoglobal
case_team <- function(df, col) {

  df |>
    dplyr::mutate(team_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Not Permitted",
      "1" ~ "Medical Necessity Documentation Required",
      "2" ~ "Permitted",
      "9" ~ NA_character_
    ),
    .after = {{ col }})
}

#' Add Bilateral Surgery Descriptions
#'
#' Modifier 50: Indicates services subject to payment adjustment
#'
#' @param df data frame
#' @param col column of Bilateral Surgery indicators
#' @return A [tibble][tibble::tibble-package] with a `bilat_description` column
#' @examples
#' dplyr::tibble(surg_bilat = c(0:3, "9")) |> case_bilateral(surg_bilat)
#' @export
#' @keywords internal
#' @autoglobal
case_bilateral <- function(df, col) {

  df |>
    dplyr::mutate(bilat_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Adjustment does not apply. If reported with mod 50 or RT and LT, payment for the two sides is the lower of (a) total charge for both sides (b) 100% of fee schedule amount for a single code. Adjustment is inappropriate because (a) of physiology or anatomy, or (b) code description states it is a unilateral procedure and there is an existing code for the bilateral procedure.",
      "1" ~ "Adjustment applies. If reported with bilateral modifier or twice on same day by any other means (with RT and LT mods, or with a 2 in the units field), base payment on lower of: (a) total charge for both sides or (b) 150% of fee schedule amount for a single code. If reported as bilateral procedure and reported with other procedure codes on same day, apply bilateral adjustment before applying any multiple procedure rules.",
      "2" ~ "Adjustment does not apply. RVUs already based on procedure as a bilateral procedure. If reported with mod -50 or twice on same day by any other means, base payment on lower of (a) total charge for both sides, or (b) 100% of fee schedule for a single code.",
      "3" ~ "Adjustment does not apply. If reported with mod 50 or for both sides on same day by any other means, base payment for each side or organ or site of paired organ on lower of (a) charge for each side or (b) 100% of fee schedule amount for each side. If reported as bilateral procedure and with other procedure codes on same day, determine fee schedule amount for a bilateral procedure before applying any multiple procedure rules. Services in this category are generally radiology procedures or other diagnostic tests which are not subject to the special payment rules for other bilateral surgeries.",
      "9" ~ NA_character_ # "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add Multiple Procedure Descriptions
#'
#' Modifier 51: Indicates applicable payment adjustment rule for multiple procedures
#'
#' @param df data frame
#' @param col column of Multiple Procedure indicators
#' @return A [tibble][tibble::tibble-package] with a `mproc_description` column
#' @examples
#' dplyr::tibble(mult_proc = as.character(0:9)) |> case_multproc(mult_proc)
#' @export
#' @keywords internal
#' @autoglobal
case_multproc <- function(df, col) {

  df |>
    dplyr::mutate(mproc_description = dplyr::case_match(
      {{ col }},
      "0" ~ "No adjustment. If procedure is reported on the same day as another procedure, base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount for the procedure.",
      "1" ~ "Standard adjustment. If reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 25%, 25%, 25%, and by report). Base payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
      "2" ~ "Standard adjustment. If reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 50%, 50%, 50% and by report). Base payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
      "3" ~ "Special rules for multiple endoscopic procedures apply if procedure is billed with another endoscopy in the same family (i.e., another endoscopy that has the same base procedure). The base procedure for each code with this indicator is identified in the Endobase column. Apply the multiple endoscopy rules to a family before ranking the family with the other procedures performed on the same day (for example, if multiple endoscopies in the same family are reported on the same day as endoscopies in another family or on the same day as a non-endoscopic procedure). If an endoscopic procedure is reported with only its base procedure, do not pay separately for the base procedure. Payment for the base procedure is included in the payment for the other endoscopy.",
      "4" ~ "Special rules for the technical component (TC) of diagnostic imaging procedures apply if procedure is billed with another diagnostic imaging procedure in the same family (per the diagnostic imaging family indicator, below). If procedure is reported in the same session on the same day as another procedure with the same family indicator, rank the procedures by fee schedule amount for the TC. Pay 100% for the highest priced procedure, and 50% for each subsequent procedure. Base the payment for subsequent procedures on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage. Subject to 50% reduction of the TC diagnostic imaging (effective for services July 1, 2010 and after). Subject to 25% reduction of the PC of diagnostic imaging (effective for services January 1, 2012 through December 31, 2016). Subject to 5% reduction of the PC of diagnostic imaging (effective for services January 1, 2017 and after).",
      "5" ~ "Subject to 50% of the practice expense component for certain therapy services.",
      "6" ~ "Subject to 25% reduction of the second highest and subsequent procedures to the TC of diagnostic cardiovascular services, effective for services January 1, 2013, and thereafter.",
      "7" ~ "Subject to 20% reduction of the second highest and subsequent procedures to the TC of diagnostic ophthalmology services, effective for services January 1, 2013, and thereafter.",
      "9" ~ NA_character_ # "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add Co-Surgeons Descriptions
#'
#' Modifier 62: Services for which two surgeons,
#' each in a different specialty, may be paid.
#'
#' @param df data frame
#' @param col column of Co Surgeon indicators
#' @return A [tibble][tibble::tibble-package] with a `cosurg_description` column
#' @examples
#' dplyr::tibble(surg_co = c(0:2, "9")) |> case_cosurg(surg_co)
#' @export
#' @autoglobal
case_cosurg <- function(df, col) {

  df |>
    dplyr::mutate(cosurg_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Not Permitted",
      "1" ~ "Medical Necessity Documentation Required",
      "2" ~ "Permitted",
      "9" ~ NA_character_ # "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add Assistant Surgery Descriptions
#'
#' Fee schedule amount equals 16 percent of amount
#' otherwise applicable for surgical payment.
#'
#' Modifiers:
#' + 80: Assistance by Another Physician
#' + 81: Minimal Assistance by a Another Physician
#' + 82: Assistance by Another Physician when Qualified Resident Surgeon Unavailable
#' + AS: Non-Physician Assistant at Surgery
#'
#' @param df data frame
#' @param col column of Assistant Surgery indicators
#' @return A [tibble][tibble::tibble-package] with a `asst_description` column
#' @examples
#' dplyr::tibble(surg_asst = c(0:2, "9")) |> case_assistant(surg_asst)
#' @export
#' @autoglobal
case_assistant <- function(df, col) {

  df |>
    dplyr::mutate(asst_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Payment Restriction unless Medical Necessity documentation submitted",
      "1" ~ "Payment Restriction; Assistant cannot be paid",
      "2" ~ "No Payment Restriction; Assistant can be paid",
      "9" ~ NA_character_ # "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add OPPS Indicator Descriptions
#'
#' @param df data frame
#' @param col column of OPPS Indicator indicators
#' @return A [tibble][tibble::tibble-package] with an `opps_description` column
#' @examples
#' dplyr::tibble(opps_ind = c("1", "9")) |> case_opps(opps_ind)
#' @export
#' @autoglobal
case_opps <- function(df, col) {

  df |>
    dplyr::mutate(opps_description = dplyr::case_match({{ col }},
    "1" ~ "Subject to OPPS Payment Cap",
    "9" ~ "Not Subject to OPPS Payment Cap"),
    .after = {{ col }})
}

#' Add Modifier Indicator Descriptions
#'
#' @param df data frame
#' @param col column of Modifier indicators
#' @return A [tibble][tibble::tibble-package] with an `mod_description` column
#' @examples
#' dplyr::tibble(mod = c(26, "TC", 53)) |> case_modifier(mod)
#' @export
#' @keywords internal
#' @autoglobal
case_modifier <- function(df, col) {

  df |>
    dplyr::mutate(mod_description = dplyr::case_match(
      {{ col }},
      "26" ~ dplyr::tibble(
        mod_label       = "Professional Component",
        mod_description = "Certain procedures are a combination of a physician or other qualified health care professional component and a technical component. When the physician or other qualified health care professional component is reported separately, the service may be identified by adding modifier 26 to the usual procedure number."),
      "TC" ~ dplyr::tibble(
        mod_label       = "Technical Component",
        mod_description = "Under certain circumstances, a charge may be made for the technical component alone. Under those circumstances the technical component charge is identified by adding modifier TC to the usual procedure number. Technical component charges are institutional charges and not billed separately by physicians; however, portable x-ray suppliers only bill for technical component and should utilize modifier TC. The charge data from portable x-ray suppliers will then be used to build customary and prevailing profiles."),
      "53" ~ dplyr::tibble(
        mod_label       = "Discontinued Procedure",
        mod_description = "Under certain circumstances, the physician or other qualified health care professional may elect to terminate a surgical or diagnostic procedure. Due to extenuating circumstances or those that threaten the well being of the patient, it may be necessary to indicate that a surgical or diagnostic procedure was started but discontinued. This circumstance may be reported by adding modifier 53 to the code reported by the individual for the discontinued procedure.")),
    .after = {{ col }}) |>
    tidyr::unpack(cols = mod_description)
}

#' Add PCTC Indicator Descriptions
#'
#' @param df data frame
#' @param col column of PCTC indicators
#' @return A [tibble][tibble::tibble-package] with a `pctc_description` column
#' @examples
#' dplyr::tibble(pctc = as.character(0:9)) |> case_pctc(pctc)
#' @export
#' @keywords internal
#' @autoglobal
case_pctc <- function(df, col) {
  df |>
    dplyr::mutate(pctc_description = dplyr::case_match(
      {{ col }},
    "0" ~ dplyr::tibble(
      pctc_label       = "Physician Service",
      pctc_description = "PCTC does not apply."),
    "1" ~ dplyr::tibble(
      pctc_label       = "Diagnostic Tests for Radiology Services",
      pctc_description = "Have both a PC and TC. Mods 26/TC can be used. RVU components: Code + Mod -26 [wRVU, pRVU, mRVU]; Code + Mod -TC [pRVU, mRVU]; Code [wRVU, pRVU, mRVU]"),
    "2" ~ dplyr::tibble(
      pctc_label       = "Professional Component Only",
      pctc_description = "Standalone code. Describes PC of diagnostic tests for which there is a code that describes TC of diagnostic test only and another code that describes the Global test. RVU components: wRVU, pRVU, mRVU"),
    "3" ~ dplyr::tibble(
      pctc_label       = "Technical Component Only",
      pctc_description = "Standalone code. Mods 26/TC cannot be used. Describe TC of diagnostic tests for which there is a code that describes PC of the diagnostic test only. Also identifies codes that are covered only as diagnostic tests and do not have a PC code. RVU components: pRVU, mRVU"),
    "4" ~ dplyr::tibble(
      pctc_label       = "Global Test Only",
      pctc_description = "Standalone code. Describes diagnostic tests for which there are codes that describe PC of the test only, and the TC of the test only. Mods 26/TC cannot be used. Total RVUs is sum of total RVUs for PC and TC only codes combined. RVU components: wRVU, pRVU, mRVU"),
    "5" ~ dplyr::tibble(
      pctc_label       = "Incident To",
      pctc_description = "Services provided by personnel working under physician supervision. Payment may not be made when provided to hospital inpatients or outpatients. Mods 26/TC cannot be used."),
    "6" ~ dplyr::tibble(
      pctc_label       = "Lab Physician Interpretation",
      pctc_description = "Clinical Lab codes for which separate payment for interpretations by laboratory physicians may be made. Actual performance of tests paid by lab fee schedule. Mod TC cannot be used. RVU components: wRVU, pRVU, mRVU"),
    "7" ~ dplyr::tibble(
      pctc_label       = "Physical Therapy",
      pctc_description = "Payment may not be made if provided to hospital outpatient/inpatient by independently practicing physical or occupational therapist."),
    "8" ~ dplyr::tibble(
      pctc_label       = "Physician Interpretation",
      pctc_description = "Identifies PC of Clinical Lab codes for which separate payment made only if physician interprets abnormal smear for hospital inpatient. No TC billing recognized, payment for test made to hospital. No payment for CPT 85060 furnished to hospital outpatients or non-hospital patients. Physician interpretation paid through clinical laboratory fee schedule."),
    "9" ~ dplyr::tibble(
      pctc_label       = NA_character_, # "Not Applicable",
      pctc_description = NA_character_ # "PCTC Concept does not apply"
      )),
    .after = {{ col }}) |>
    tidyr::unpack(cols = pctc_description)
}

#' Add Status Code Descriptions
#'
#' @param df data frame
#' @param col column of Status Codes
#' @return A [tibble][tibble::tibble-package] with an `status_description` column
#' @examples
#' dplyr::tibble(status = LETTERS) |> case_status(status)
#' @export
#' @keywords internal
#' @autoglobal
case_status <- function(df, col) {

  df |>
    dplyr::mutate(status_description = dplyr::case_match(
      {{ col }},
    "A" ~ dplyr::tibble(status_label       = "Active",
                        status_description = "Separately paid if covered. RVUs and payment amounts. Carriers responsible for coverage decisions in absence of an NCD."),
    "B" ~ dplyr::tibble(status_label       = "Payment Bundled",
                        status_description = "Payment bundled into payment for other services not specified. No RVUs, no payment made. When covered, payment subsumed by payment for services to which they are incident."),
    "C" ~ dplyr::tibble(status_label       = "Carrier Priced",
                        status_description = "Carriers establish RVUs and payment following documentation review."),
    "D" ~ dplyr::tibble(status_label       = "Deleted Codes",
                        status_description = "Deleted effective with beginning of year."),
    "E" ~ dplyr::tibble(status_label       = "Regulatory Exclusion",
                        status_description = "Excluded by regulation. No RVUs, no payment made. When covered, payment made under reasonable charge procedures."),
    "F" ~ dplyr::tibble(status_label       = "Deleted/Discontinued",
                        status_description = "Not subject to 90 day grace period"),
    "G" ~ dplyr::tibble(status_label       = "Not Valid for Medicare",
                        status_description = "Another code used for payment. Subject to a 90 day grace period."),
    "H" ~ dplyr::tibble(status_label       = "Deleted Modifier",
                        status_description = "Had TC/26 mod in previous year, TC/26 component now deleted."),
    "I" ~ dplyr::tibble(status_label       = "Not Valid for Medicare",
                        status_description = "Another code used for payment. Not subject to a 90-day grace period."),
    "J" ~ dplyr::tibble(status_label       = "Anesthesia Service",
                        status_description = "No RVUs or payment amounts. Only identifies anesthesia services."),
    "M" ~ dplyr::tibble(status_label       = "Measurement Code",
                        status_description = "Used for reporting purposes only."),
    "N" ~ dplyr::tibble(status_label       = "Restricted Coverage",
                        status_description = "Not covered by Medicare."),
    "P" ~ dplyr::tibble(status_label       = "Non-Covered Service",
                        status_description = "No RVUs, no payment made. If covered as Incident To and provided on same day as physician service, payment bundled into payment for Incident To service. If covered as other than Incident To, paid under other payment provision."),
    "R" ~ dplyr::tibble(status_label       = "Bundled/Excluded Code",
                        status_description = "Special coverage instructions apply. If covered, service is contractor priced. Assigned to limited number of codes covered in unusual circumstances. Majority of codes are dental codes."),
    "T" ~ dplyr::tibble(status_label       = "Injections",
                        status_description = "RVUs and payment amounts. Paid only if no other payable services billed on same date by same provider. If payable services billed, bundled into payment."),
    "X" ~ dplyr::tibble(status_label       = "Statutory Exclusion",
                        status_description = "Not in statutory definition of Physician Services. No RVUs or payment amounts, no payment made.")
    ), .after = {{ col }}) |>
    tidyr::unpack(cols = status_description)
}
