#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_section` column
#' @examples
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_cpt_section(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_cpt_section <- function(df, col) {

  df |>
    dplyr::mutate(cpt_section = dplyr::case_match(
      {{ col }},
      as.character(c(99202:99499)) ~ "E&M",
      as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology",
      as.character(c(10004:69990)) ~ "Surgery",
      as.character(c(70010:79999)) ~ "Radiology",
      as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Path & Lab",
      as.character(c(90281:99199, 99500:99607)) ~ "Medicine"),
      .after = {{ col }})
}

#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `hcpcs_section` column
#' @examples
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_hcpcs_section(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_hcpcs_section <- function(df, col) {

  df |>
    dplyr::mutate(hcpcs_section = dplyr::case_match(
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
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_hcpcs_level(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_hcpcs_level <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(hcpcs_level = dplyr::case_when(
      is_level_I({{ col }}) ~ "I",
      is_level_II({{ col }}) ~ "II"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}

#' Add CPT Category Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `cpt_category` column
#' @examplesIf interactive()
#' x <- c("39503", "99215", "99140", "69990", "70010",
#'        "0222U", "V5299", "7010F", "0074T")
#' dplyr::tibble(hcpcs = x) |>
#' case_cpt_category(hcpcs)
#' @export
#' @keywords internal
#' @autoglobal
case_cpt_category <- function(df, col) {

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(cpt_category = dplyr::case_when(
      is_category_I({{ col }}) ~ "I",
      is_category_II({{ col }}) ~ "II",
      is_category_III({{ col }}) ~ "III"),
      .after = {{ col }}) |>
    dplyr::ungroup()
}

#' Add Global Days Descriptions
#' @param df data frame
#' @param col column of Global Days indicators
#' @return A [tibble][tibble::tibble-package] with a `global_description` column
#' @examples
#' dplyr::tibble(global_days = c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ")) |>
#' case_global_days(global_days)
#' @export
#' @keywords internal
#' @autoglobal
case_global_days <- function(df, col) {

  df |>
    dplyr::mutate(global_description = dplyr::case_match(
      {{ col }},
      "000" ~ "Endoscopic or minor procedure with related Preoperative and Postoperative RVUs on the day of the procedure only included in the fee schedule payment amount. E&M services on the day of the procedure generally not payable.",
      "010" ~ "Minor procedure with Preoperative RVUs on the day of the procedure and Postoperative RVUs during a 10-day postoperative period included in the fee schedule amount. E&M services on the day of the procedure and during the 10-day postoperative period generally not payable.",
      "090" ~ "Major surgery with a 1-day Preoperative period and 90-day Postoperative period included in fee schedule amount.",
      "MMM" ~ "Maternity codes. Usual Global period does not apply.",
      "XXX" ~ "Global concept does not apply.",
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
#'               case_diagnostic_imaging(dximg)
#' @export
#' @keywords internal
#' @autoglobal
case_diagnostic_imaging <- function(df, col) {

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
      "99" ~ "Concept does not apply"
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
#' x <- stringr::str_pad(c(1:6, 9, 21, 22, 66, "6A", 77, "7A"),
#'                       width = "2", pad = "0")
#' dplyr::tibble(supvis = x) |>
#' case_physician_supervision(supvis)
#' @export
#' @keywords internal
#' @autoglobal
case_physician_supervision <- function(df, col) {

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
      "09" ~ "Concept does not apply"
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
#' dplyr::tibble(surg_team = c(0:2, "9")) |>
#' case_team_surgery(surg_team)
#' @export
#' @keywords internal
#' @autoglobal
case_team_surgery <- function(df, col) {

  df |>
    dplyr::mutate(team_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Not Permitted",
      "1" ~ "Medical Necessity Documentation Required",
      "2" ~ "Permitted",
      "9" ~ "Concept does not apply"
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
      "0" ~ "150% adjustment does not apply. If reported with modifier -50 or RT and LT, base payment for the two sides on lower of: (a) the total actual charge for both sides and (b) 100% of the fee schedule amount for a single code. The bilateral adjustment is inappropriate for codes in this category (a) because of physiology or anatomy, or (b) because the code description specifically states that it is a unilateral procedure and there is an existing code for the bilateral procedure.",
      "1" ~ "150% adjustment applies. If billed with bilateral modifier or reported twice on same day by any other means (e.g., with RT and LT mods, or with a 2 in the units field), base the payment for these codes when reported as bilateral procedures on the lower of: (a) the total actual charge for both sides or (b) 150% of the fee schedule amount for a single code. If the code is reported as a bilateral procedure and is reported with other procedure codes on the same day, apply the bilateral adjustment before applying any multiple procedure rules.",
      "2" ~ "150% adjustment does not apply. RVUs are already based on procedure as a bilateral procedure. If reported with mod -50 or reported twice on same day by any other means (e.g., with RT and LT modifiers or with a 2 in the units field), base the payment for both sides on the lower of (a) the total actual charge by the physician for both sides, or (b) 100% of the fee schedule for a single code. ",
      "3" ~ "Usual payment adjustment does not apply. If reported with mod 50 or reported for both sides on same day by any other means (e.g., with RT and LT modifiers or with a 2 in the units field), base the payment for each side or organ or site of a paired organ on the lower of (a) the actual charge for each side or (b) 100% of the fee schedule amount for each side. If the procedure is reported as a bilateral procedure and with other procedure codes on the same day, determine the fee schedule amount for a bilateral procedure before applying any multiple procedure rules. Services in this category are generally radiology procedures or other diagnostic tests which are not subject to the special payment rules for other bilateral surgeries.",
      "9" ~ "Concept does not apply"
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
      "0" ~ "No payment adjustment rules for multiple procedures apply. If procedure is reported on the same day as another procedure, base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount for the procedure.",
      "1" ~ "Standard payment adjustment rules in effect before January 1, 1995 for multiple procedures apply. In the 1995 file, this indicator only applies to codes with a status code of D. If procedure is reported on the same day as another procedure that has an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 25%, 25%, 25%, and by report). Base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
      "2" ~ "Standard payment adjustment rules for multiple procedures apply. If procedure is reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 50%, 50%, 50% and by report). Base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
      "3" ~ "Special rules for multiple endoscopic procedures apply if procedure is billed with another endoscopy in the same family (i.e., another endoscopy that has the same base procedure). The base procedure for each code with this indicator is identified in the Endobase field of this file. Apply the multiple endoscopy rules to a family before ranking the family with the other procedures performed on the same day (for example, if multiple endoscopies in the same family are reported on the same day as endoscopies in another family or on the same day as a non-endoscopic procedure). If an endoscopic procedure is reported with only its base procedure, do not pay separately for the base procedure. Payment for the base procedure is included in the payment for the other endoscopy.",
      "4" ~ "Special rules for the technical component (TC) of diagnostic imaging procedures apply if procedure is billed with another diagnostic imaging procedure in the same family (per the diagnostic imaging family indicator, below). If procedure is reported in the same session on the same day as another procedure with the same family indicator, rank the procedures by fee schedule amount for the TC. Pay 100% for the highest priced procedure, and 50% for each subsequent procedure. Base the payment for subsequent procedures on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage. Subject to 50% reduction of the TC diagnostic imaging (effective for services July 1, 2010 and after). Subject to 25% reduction of the PC of diagnostic imaging (effective for services January 1, 2012 through December 31, 2016). Subject to 5% reduction of the PC of diagnostic imaging (effective for services January 1, 2017 and after).",
      "5" ~ "Subject to 50% of the practice expense component for certain therapy services.",
      "6" ~ "Subject to 25% reduction of the second highest and subsequent procedures to the TC of diagnostic cardiovascular services, effective for services January 1, 2013, and thereafter.",
      "7" ~ "Subject to 20% reduction of the second highest and subsequent procedures to the TC of diagnostic ophthalmology services, effective for services January 1, 2013, and thereafter.",
      "9" ~ "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add Co Surgeons Descriptions
#'
#' Modifier 62: Services for which two surgeons,
#' each in a different specialty, may be paid.
#'
#' @param df data frame
#' @param col column of Co Surgeon indicators
#' @return A [tibble][tibble::tibble-package] with a `cosurg_description` column
#' @examples
#' dplyr::tibble(surg_co = c(0:2, "9")) |> case_team_surgery(surg_co)
#' @export
#' @keywords internal
#' @autoglobal
case_co_surgeon <- function(df, col) {

  df |>
    dplyr::mutate(cosurg_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Not Permitted",
      "1" ~ "Medical Necessity Documentation Required",
      "2" ~ "Permitted",
      "9" ~ "Concept does not apply"
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
#' dplyr::tibble(surg_asst = c(0:2, "9")) |> case_assistant_surgery(surg_asst)
#' @export
#' @keywords internal
#' @autoglobal
case_assistant_surgery <- function(df, col) {

  df |>
    dplyr::mutate(asst_description = dplyr::case_match(
      {{ col }},
      "0" ~ "Payment Restriction unless Medical Necessity documentation submitted",
      "1" ~ "Payment Restriction; Assistant cannot be paid",
      "2" ~ "No Payment Restriction; Assistant can be paid",
      "9" ~ "Concept does not apply"
    ),
    .after = {{ col }})
}

#' Add OPPS Indicator Descriptions
#'
#' @param df data frame
#' @param col column of OPPS Indicator indicators
#' @return A [tibble][tibble::tibble-package] with an `opps_description` column
#' @examples
#' dplyr::tibble(opps_ind = c("1", "9")) |> case_opps_ind(opps_ind)
#' @export
#' @keywords internal
#' @autoglobal
case_opps_ind <- function(df, col) {

  df |>
    dplyr::mutate(opps_description = dplyr::case_match({{ col }},
    "1" ~ "OPPS Payment Cap",
    "9" ~ "No OPPS Payment Cap"),
    .after = {{ col }})
}

#' Add Modifier Indicator Descriptions
#'
#' @param df data frame
#' @param col column of Modifier indicators
#' @return A [tibble][tibble::tibble-package] with an `mod_description` column
#' @examples
#' dplyr::tibble(mod = c("26", "TC", "53)) |> case_modifier(mod)
#' @export
#' @keywords internal
#' @autoglobal
case_modifier <- function(df, col) {

  df |>
    dplyr::mutate(mod_description = dplyr::case_match(
      {{ col }},
      "26" ~ dplyr::tibble(mod_label = "Professional Component", mod_description = "Certain procedures are a combination of a physician or other qualified health care professional component and a technical component. When the physician or other qualified health care professional component is reported separately, the service may be identified by adding modifier 26 to the usual procedure number."),
      "TC" ~ dplyr::tibble(mod_label = "Technical Component", mod_description = "Under certain circumstances, a charge may be made for the technical component alone. Under those circumstances the technical component charge is identified by adding modifier TC to the usual procedure number. Technical component charges are institutional charges and not billed separately by physicians; however, portable x-ray suppliers only bill for technical component and should utilize modifier TC. The charge data from portable x-ray suppliers will then be used to build customary and prevailing profiles."),
      "53" ~ dplyr::tibble(mod_label = "Discontinued Procedure", mod_description = "Under certain circumstances, the physician or other qualified health care professional may elect to terminate a surgical or diagnostic procedure. Due to extenuating circumstances or those that threaten the well being of the patient, it may be necessary to indicate that a surgical or diagnostic procedure was started but discontinued. This circumstance may be reported by adding modifier 53 to the code reported by the individual for the discontinued procedure.")
    ),
    .after = {{ col }})
}

#' Add PCTC Indicator Descriptions
#'
#' @param df data frame
#' @param col column of PCTC indicators
#' @return A [tibble][tibble::tibble-package] with an `pctc_description` column
#' @examples
#' dplyr::tibble(pctc = as.character(0:9)) |> case_pctc(pctc)
#' @export
#' @keywords internal
#' @autoglobal
case_pctc <- function(df, col) {
  df |>
    dplyr::mutate(pctc_description = dplyr::case_match(
      {{ col }},
    "0" ~ dplyr::tibble(label = "Physician Service", description = "PCTC concept does not apply. Cannot be split into PCTC components. Mods 26 and TC cannot be used. RVU components: wRVU, pRVU, mRVU."),
    "1" ~ dplyr::tibble(label = "Diagnostic Tests for Radiology Services", description = "Have both a PC and TC. Mods 26 and TC can be used. RVU components: Code + 26 [wRVU, pRVU, mRVU];  Code + TC [pRVU, mRVU]; Code + No Mod [wRVU, pRVU, mRVU]"),
    "2" ~ dplyr::tibble(label = "Professional Component Only", description = "Standalone codes, describe physician work portion of selected diagnostic tests for which there is an associated code that describes the technical component of the diagnostic test only and another associated code that describes the global test. The total RVUs for professional component only codes include values for physician work, practice expense, and malpractice expense."),
    "3" ~ dplyr::tibble(label = "Technical Component Only", description = "Standalone codes that describe the technical component (i.e., staff and equipment costs) of selected diagnostic tests for which there is an associated code that describes the professional component of the diagnostic test only. Also identifies codes that are covered only as diagnostic tests and therefore do not have a related professional code. Modifiers 26 and TC cannot be used with these codes. The total RVUs for technical component only codes include values for practice expense and malpractice expense only."),
    "4" ~ dplyr::tibble(label = "Global Test Only", description = "Standalone codes that describe selected diagnostic tests for which there are associated codes that describe 1. the professional component of the test only, and 2. the technical component of the test only. Modifiers 26 and TC cannot be used with these codes. The total RVUs for global procedure only codes include values for physician work, practice expense, and malpractice expense. The total RVUs for global procedure only codes equals the sum of the total RVUs for the professional and technical components only codes combined."),
    "5" ~ dplyr::tibble(label = "Incident To", description = "Services covered incident to a physician's service when they are provided by auxiliary personnel employed by the physician and working under his or her direct personal supervision. Payment may not be made by A/B MACs (B) for these services when they are provided to hospital inpatients or patients in a hospital outpatient department. Modifiers 26 and TC cannot be used with these codes."),
    "6" ~ dplyr::tibble(label = "Laboratory Physician Interpretation", description = "Clinical laboratory codes for which separate payment for interpretations by laboratory physicians may be made. Actual performance of the tests is paid for under the lab fee schedule. Modifier TC cannot be used with these codes. The total RVUs for laboratory physician interpretation codes include values for physician work, practice expense, and malpractice expense."),
    "7" ~ dplyr::tibble(label = "Physical Therapy", description = "Payment may not be made if the service is provided to either a patient in a hospital outpatient department or to an inpatient of the hospital by an independently practicing physical or occupational therapist."),
    "8" ~ dplyr::tibble(label = "Physician Interpretation", description = "Identifies PC of Clinical Laboratory codes for which separate payment may be made only if the physician interprets an abnormal smear for hospital inpatient. This applies to CPT codes 85060. No TC billing is recognized because payment for the underlying clinical laboratory test is made to the hospital, generally through the PPS rate. No payment is recognized for CPT codes 85060 furnished to hospital outpatients or non-hospital patients. The physician interpretation is paid through the clinical laboratory fee schedule payment for the clinical laboratory test."),
    "9" ~ dplyr::tibble(label = "Not Applicable", description = "PCTC Concept does not apply")), .after = {{ col }})
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
    "A" ~ dplyr::tibble(label = "Active Code", description = "Separately paid under the Physician Fee Schedule if covered. There will be RVUs and payment amounts. Does not mean that Medicare has made a National Coverage Determination regarding the service. Carriers remain responsible for coverage decisions in the absence of a national Medicare policy."),
    "B" ~ dplyr::tibble(label = "Payment Bundled", description = "Payment for covered services are always bundled into payment for other services not specified. No RVUs or payment amounts and no separate payment is ever made. When these services are covered, payment for them is subsumed by the payment for the services to which they are incident. Example: telephone call from a hospital nurse regarding care of a patient."),
    "C" ~ dplyr::tibble(label = "Carrier Priced", description = "Carriers will establish RVUs and payment amounts for these services, generally on an individual case basis following review of documentation such as an operative report."),
    "D" ~ dplyr::tibble(label = "Deleted Codes", description = "Deleted effective with the beginning of the applicable year."),
    "E" ~ dplyr::tibble(label = "Regulatory Exclusion", description = "Item or service that CMS chose to exclude from the fee schedule payment by regulation. No RVUs or payment amounts are shown and no payment may be made under the fee schedule. Payment for them, when covered, continues under reasonable charge procedures."),
    "F" ~ dplyr::tibble(label = "Deleted/Discontinued Codes", description = "Code not subject to a 90 day grace period"),
    "G" ~ dplyr::tibble(label = "Not Valid for Medicare Purposes", description = "Medicare uses another code for reporting of, and payment for, these services. Code subject to a 90 day grace period."),
    "H" ~ dplyr::tibble(label = "Deleted Modifier", description = "Had an associated TC and/or 26 modifier in the previous year. For the current year, the TC or 26 component shown for the code has been deleted, and the deleted component is shown with a status code of H."),
    "I" ~ dplyr::tibble(label = "Not Valid for Medicare Purposes", description = "Medicare uses another code for reporting of, and payment for, these services. Code is NOT subject to a 90-day grace period."),
    "J" ~ dplyr::tibble(label = "Anesthesia Service", description = "No RVUs or payment amounts for anesthesia codes on the database, only used to facilitate the identification of anesthesia services."),
    "M" ~ dplyr::tibble(label = "Measurement Code", description = "Used for reporting purposes only."),
    "N" ~ dplyr::tibble(label = "Restricted Coverage", description = "Not covered by Medicare."),
    "P" ~ dplyr::tibble(label = "Non-Covered Service", description = "No RVUs and no payment amounts for these services. No separate payment is made for them under the fee schedule. If the item or service is covered as incident to a physician service and is provided on the same day as a physician service, payment for it is bundled into the payment for the physician service to which it is incident (an example is an elastic bandage furnished by a physician incident to a physician service). If the item or service is covered as other than incident to a physician service, it is excluded from the fee schedule (for example, colostomy supplies) and is paid under the other payment provision of the Act."),
    "R" ~ dplyr::tibble(label = "Bundled/Excluded Code", description = "Special coverage instructions apply. If covered, the service is contractor priced. NOTE: The majority of codes to which this indicator will be assigned are the alpha-numeric dental codes, which begin with D. We are assigning the indicator to a limited number of CPT codes which represent services that are covered only in unusual circumstances."),
    "T" ~ dplyr::tibble(label = "Injections", description = "There are RVUs and payment amounts for these services, but they are only paid if there are no other services payable under the physician fee schedule billed on the same date by the same provider. If any other services payable under the physician fee schedule are billed on the same date by the same provider, these services are bundled into the physician services for which payment is made. NOTE: This is a change from the previous definition, which states that injection services are bundled into any other services billed on the same date."),
    "X" ~ dplyr::tibble(label = "Statutory Exclusion", description = "Item or service that is not in the statutory definition of 'physician services' for fee schedule payment purposes. No RVUs or payment amounts are shown for these codes and no payment may be made under the physician fee schedule. Ex: Ambulance Services and Clinical Diagnostic Laboratory Services.")
    ), .after = {{ col }})
}
