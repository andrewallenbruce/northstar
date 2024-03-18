#' Recoding Functions
#'
#' @description Various recoding functions
#'
#' @examplesIf interactive()
#' cd <- dplyr::tibble(
#'       hcpcs = c("39503", "99215", "99140",
#'                 "69990", "70010", "0222U",
#'                 "V5299", "7010F", "0074T")
#'
#' cd |> case_section(hcpcs)
#' cd |> case_section_cpt(hcpcs)
#' cd |> case_section_hcpcs(hcpcs)
#' cd |> case_level(hcpcs)
#' cd |> case_category(hcpcs)
#'
#' # Global Days
#' glob <- c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ")
#' dplyr::tibble(global = glob) |>
#' case_global(global)
#'
#'
#' # Diagnostic Imaging Family
#' di <- stringr::str_pad(
#'       c(1:11, 88, 99), width = "2", pad = "0")
#' dplyr::tibble(dximg = di) |>
#' case_imaging(dximg)
#'
#' # Physician Supervision
#' sv <- stringr::str_pad(
#'       c(1:6, 9, 21:22, 66, "6A", 77, "7A"), width = "2", pad = "0")
#' dplyr::tibble(supvis = sv) |>
#' case_supervision(supvis)
#'
#' # Team Surgery
#' dplyr::tibble(surg_team = c(0:2, "9")) |>
#' case_team(surg_team)
#'
#' # Bilateral Surgery
#' dplyr::tibble(surg_bilat = c(0:3, "9")) |>
#' case_bilateral(surg_bilat)
#'
#' # Multiple Procedure
#' dplyr::tibble(mult_proc = as.character(0:9)) |>
#' case_multproc(mult_proc)
#'
#' # Co-Surgeons
#' dplyr::tibble(surg_co = c(0:2, "9")) |>
#' case_cosurg(surg_co)
#'
#' # Assistant at Surgery
#' dplyr::tibble(surg_asst = c(0:2, "9")) |>
#' case_assistant(surg_asst)
#'
#' # OPPS Indicator
#' dplyr::tibble(opps_ind = c("1", "9")) |>
#' case_opps(opps_ind)
#'
#' # Modifier Indicator
#' dplyr::tibble(mod = c(26, "TC", 53)) |>
#' case_modifier(mod)
#'
#' # PCTC Indicator
#' dplyr::tibble(pctc = as.character(0:9)) |>
#' case_pctc(pctc)
#'
#' # Status Codes
#' dplyr::tibble(status = LETTERS) |>
#' case_status(status, desc = TRUE)
#'
#' @name cases
NULL

#' Add HCPCS Level I & II Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `section` column
#' @export
#' @autoglobal
#' @rdname cases
case_section <- function(df, col) {

  df |>
    dplyr::mutate(section = dplyr::case_match(
      {{ col }},
      as.character(c(99202:99499)) ~ "E&M [99202-99499]",
      as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology [00100-01999, 99100-99140]",
      as.character(c(10004:69990)) ~ "Surgery [10004-69990]",
      as.character(c(70010:79999)) ~ "Radiology [70010-79999]",
      as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Path & Lab [80047-89398, 0001U-0222U]",
      as.character(c(90281:99199, 99500:99607)) ~ "Medicine [90281-99199, 99500-99607]",
      .default = NA_character_
      )) |>
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
      "V" ~ "Vision/Hearing Services",
      .default = section
    ),
    .after = {{ col }})
}

#' Add CPT Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `section` column
#' @export
#' @autoglobal
#' @rdname cases
case_section_cpt <- function(df, col) {

  df |>
    dplyr::mutate(section = dplyr::case_match(
      {{ col }},
      as.character(c(99202:99499)) ~ "E&M [99202-99499]",
      as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology [00100-01999, 99100-99140]",
      as.character(c(10004:69990)) ~ "Surgery [10004-69990]",
      as.character(c(70010:79999)) ~ "Radiology [70010-79999]",
      as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Path & Lab [80047-89398, 0001U-0222U]",
      as.character(c(90281:99199, 99500:99607)) ~ "Medicine [90281-99199, 99500-99607]",
      .default = NA_character_
      ),
      .after = {{ col }})
}

#' Add HCPCS Section Labels
#' @param df data frame
#' @param col column of HCPCS codes to match on
#' @return A [tibble][tibble::tibble-package] with a `section` column
#' @export
#' @autoglobal
#' @rdname cases
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
#' @return A [tibble][tibble::tibble-package] with a `level` column
#' @export
#' @autoglobal
#' @rdname cases
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
#' @return A [tibble][tibble::tibble-package] with a `category` column
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
case_global <- function(df, col) {

  df |>
    dplyr::mutate(global_description = dplyr::case_match(
      {{ col }},
      "000" ~ "Endoscopic or minor procedure with related Preoperative and Postoperative RVUs on the day of the procedure only included in the fee schedule payment amount. E&M services on the day of the procedure generally not payable.",
      "010" ~ "Minor procedure with Preoperative RVUs on the day of the procedure and Postoperative RVUs during a 10-day postoperative period included in the fee schedule amount. E&M services on the day of the procedure and during the 10-day postoperative period generally not payable.",
      "090" ~ "Major surgery with a 1-day Preoperative period and 90-day Postoperative period included in fee schedule amount.",
      "MMM" ~ "Maternity codes. Usual Global period does not apply.",
      "XXX" ~ NA_character_, # "Global concept does not apply.",
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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
#' @export
#' @autoglobal
#' @rdname cases
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

#' Replace Status Codes with Labels, Add Descriptions
#'
#' @param df data frame
#' @param col column of Status Codes
#' @param desc add Descriptions
#' @return A [tibble][tibble::tibble-package] with a `status_desc` column
#' @export
#' @autoglobal
#' @rdname cases
case_status <- function(df, col, desc = FALSE) {

  if (desc) {

  df <- dplyr::mutate(df,
      status_desc = dplyr::case_match(
        {{ col }},
      "A" ~ "Separately paid if covered. RVUs and payment amounts. Carriers responsible for coverage decisions in absence of an NCD.",
      "B" ~ "Payment bundled into payment for other services not specified. No RVUs, no payment made. When covered, payment subsumed by payment for services to which they are incident.",
      "C" ~ "Carriers establish RVUs and payment following documentation review.",
      "D" ~ "Deleted effective with beginning of year.",
      "E" ~ "Excluded by regulation. No RVUs, no payment made. When covered, payment made under reasonable charge procedures.",
      "F" ~ "Not subject to 90 day grace period",
      "G" ~ "Another code used for payment. Subject to a 90 day grace period.",
      "H" ~ "Had TC/26 mod in previous year, TC/26 component now deleted.",
      "I" ~ "Another code used for payment. Not subject to a 90-day grace period.",
      "J" ~ "No RVUs or payment amounts. Only identifies anesthesia services.",
      "M" ~ "Used for reporting purposes only.",
      "N" ~ "Not covered by Medicare.",
      "P" ~ "No RVUs, no payment made. If covered as Incident To and provided on same day as physician service, payment bundled into payment for Incident To service. If covered as other than Incident To, paid under other payment provision.",
      "R" ~ "Special coverage instructions apply. If covered, service is contractor priced. Assigned to limited number of codes covered in unusual circumstances. Majority of codes are dental codes.",
      "T" ~ "RVUs and payment amounts. Paid only if no other payable services billed on same date by same provider. If payable services billed, bundled into payment.",
      "X" ~ "Not in statutory definition of Physician Services. No RVUs or payment amounts, no payment made."),
      .after = {{ col }}
      )
  }

  df |>
    dplyr::mutate({{ col }} := dplyr::case_match({{ col }},
    "A" ~ "Active",
    "B" ~ "Payment Bundle",
    "C" ~ "Carrier Priced",
    "D" ~ "Deleted Codes",
    "E" ~ "Regulatory Exclusion",
    "F" ~ "Deleted/Discontinued",
    "G" ~ "Not Valid for Medicare",
    "H" ~ "Deleted Modifier",
    "I" ~ "Not Valid for Medicare",
    "J" ~ "Anesthesia Service",
    "M" ~ "Measurement Code",
    "N" ~ "Restricted Coverage",
    "P" ~ "Non-Covered Service",
    "R" ~ "Bundled/Excluded Code",
    "T" ~ "Injections",
    "X" ~ "Statutory Exclusion"
    ))

}

#' Replace HCPCS Level II ASC Group Indicator Descriptions
#'
#' @param df data frame
#' @param col column of ASC Group indicators
#' @return A [tibble][tibble::tibble-package] with a `asc_description` column
#' @examples
#' dplyr::tibble(asc_grp = c("YY", "99")) |> case_asc(asc_grp)
#' @export
#' @autoglobal
#' @rdname cases
case_asc <- function(df, col) {
  dplyr::mutate(df,
  {{ col }} := dplyr::case_match({{ col }}, "YY" ~ "Approved For ASCs"))
}

#' Replace HCPCS Level II Coverage Indicators with Descriptions
#'
#' @param df data frame
#' @param col column of Coverage indicators
#' @return A [tibble][tibble::tibble-package] with a `cov_description` column
#' @examples
#' dplyr::tibble(cov = c("C", "D", "I", "M", "S")) |> case_coverage(cov)
#' @export
#' @autoglobal
#' @rdname cases
case_coverage <- function(df, col) {

  dplyr::mutate(df, {{ col }} := dplyr::case_match({{ col }},
        "C" ~ "Carrier Judgment",
        "D" ~ "Special Coverage Instructions Apply",
        "I" ~ "Not Payable by Medicare",
        "M" ~ "Not Covered by Medicare",
        "S" ~ "Not Covered by Medicare Statute"
        ))
}

#' Add HCPCS Level II Pricing Indicator Descriptions
#'
#' @param df data frame
#' @param col column of Pricing indicators
#' @return A [tibble][tibble::tibble-package] with a `price_description` column
#' @examples
#' dplyr::tibble(price = c("00", 11:13, 21:22, 31:46, 51:57, 99)) |>
#' case_pricing(price)
#' @export
#' @autoglobal
#' @rdname cases
case_pricing <- function(df, col) {

  df |>
    dplyr::mutate(
      pricing_indicator = dplyr::case_match(
        {{ col }},
        "00" ~ "Not Separately Priced by Part B",
        "11" ~ "Priced with National RVUs",
        "12" ~ "Priced with National Anesthesia Base Units",
        c("13", "22", "46", "57") ~ "Carrier Priced",
        "21" ~ "Price Subject to National Limitation Amount",
        "31" ~ "Frequently Serviced DME (Subject to Floors & Ceilings)",
        "32" ~ "Inexpensive & Routinely Purchased DME (Subject to Floors & Ceilings)",
        "33" ~ "Oxygen & Oxygen Equipment (Subject to Floors & Ceilings)",
        "34" ~ "DME Supplies (Subject to Floors & Ceilings)",
        "35" ~ "Surgical dressings (Subject to Floors & Ceilings)",
        "36" ~ "Capped Rental DME (Subject to Floors & Ceilings)",
        "37" ~ "Ostomy, Tracheostomy & Urological Supplies (Subject to Floors & Ceilings)",
        "38" ~ "Orthotics, prosthetics, prosthetic devices & vision services (Subject to Floors & Ceilings)",
        "39" ~ "Parenteral & Enteral Nutrition",
        "40" ~ "Lymphedema Compression Treatment Items",
        "45" ~ "Customized DME Items",
        "51" ~ "Drugs",
        "52" ~ "Reasonable Charge",
        "53" ~ "Statute",
        "54" ~ "Vaccinations",
        "55" ~ "Splints & Casts",
        "56" ~ "IOLs Inserted in Physician Office",
        "99" ~ "Value Not Established",
        "11:21" ~ "Priced with National RVUs, Subject to National Limitation Amount",
        "13:21" ~ "Carrier Priced, Subject to National Limitation Amount",
        "57:21" ~ "Carrier Priced, Subject to National Limitation Amount"
      ),
      .after = {{ col }})
}


#' Add HCPCS Level II Multiple Pricing Indicator Descriptions
#'
#' @param df data frame
#' @param col column of Multiple Pricing indicators
#' @return A [tibble][tibble::tibble-package] with a `mult_description` column
#' @examples
#' dplyr::tibble(mult_pi = c(9, LETTERS[1:7])) |> case_multiple_pricing(mult_pi)
#' @export
#' @autoglobal
#' @rdname cases
case_multiple_pricing <- function(df, col) {

  df |>
    dplyr::mutate(
      multiple_pricing_indicator = dplyr::case_match(
        {{ col }},
        "9" ~ "Not Applicable. Not Priced Separately by Part B or Value Not Established.",
        "A" ~ "Not Applicable. Priced Under One Methodology.",
        "B" ~ "Professional Component Priced with RVUs. Technical Component & Global Service Priced by Part B Carriers.",
        "C" ~ "Physician Interpretation Priced with PFS RVUs. Lab Service Paid under Clinical Lab Fee Schedule.",
        c("D", "E", "F") ~ "Not Applicable as of 1998-01-01.",
        "G" ~ "Submitted on Claim with Blood Products - Priced Under Reasonable Charge. No Blood Products - Priced Under Clinical Lab Fee Schedule."
      ),
      .after = {{ col }})
}

#' Add HCPCS Level II Lab Certification Descriptions
#'
#' @param df data frame
#' @param col column of Lab Certification indicators
#' @return A [tibble][tibble::tibble-package] with a `labcert_description` column
#' @examples
#' x <- c("010", 100, 110, 115, 120, "110, 120, 130, 400")
#' dplyr::tibble(labcert = x) |> case_labcert(labcert)
#' @export
#' @autoglobal
#' @rdname cases
case_labcert <- function(df, col) {

  df |>
    dplyr::mutate(
      labcert_description = dplyr::case_match(
        {{ col }},
        "010" ~ "Histocompatibility Testing",
        "100" ~ "Microbiology",
        "110" ~ "Bacteriology",
        "115" ~ "Mycobacteriology",
        "120" ~ "Mycology",
        "130" ~ "Parasitology",
        "140" ~ "Virology",
        "150" ~ "Other Microbiology",
        "200" ~ "Diagnostic Immunology",
        "210" ~ "Syphilis Serology",
        "220" ~ "General Immunology",
        "300" ~ "Chemistry",
        "310" ~ "Routine Chemistry",
        "320" ~ "Urinalysis",
        "330" ~ "Endocrinology",
        "340" ~ "Toxicology",
        "350" ~ "Other Chemistry",
        "400" ~ "Hematology",
        "500" ~ "Immunohematology",
        "510" ~ "ABO Group & RH Type",
        "520" ~ "Antibody Detection (Transfusion)",
        "530" ~ "Antibody Detection (Nontransfusion)",
        "540" ~ "Antibody Identification",
        "550" ~ "Compatibility Testing",
        "560" ~ "Other Immunohematology",
        "600" ~ "Pathology",
        "610" ~ "Histopathology",
        "620" ~ "Oral Pathology",
        "630" ~ "Cytology",
        "800" ~ "Radiobioassay",
        "900" ~ "Clinical Cytogenetics",
        "110, 120, 130, 400" ~ "Bacteriology, Mycology, Parasitology, Hematology",
        "140, 220" ~ "Virology, General Immunology",
        "220, 310, 900" ~ "General Immunology, Routine Chemistry, Clinical Cytogenetics",
        "220, 400" ~ "General Immunology, Hematology",
        "220, 610" ~ "General Immunology, Histopathology",
        "310, 400" ~ "Routine Chemistry, Hematology",
      ),
      .after = {{ col }})
}

#' Add HCPCS Level II Type of Service Descriptions
#'
#' @param df data frame
#' @param col column of Type of Service codes
#' @return A [tibble][tibble::tibble-package] with a `tos_description` column
#' @examples
#' dplyr::tibble(tos = c(0:9, LETTERS)) |> case_tos(tos)
#' @export
#' @autoglobal
#' @rdname cases
case_tos <- function(df, col) {

  df |>
    dplyr::mutate(
      type_of_service = dplyr::case_match(
        {{ col }},
        "1" ~ "Medical Care",
        "2" ~ "Surgery",
        "3" ~ "Consultation",
        "4" ~ "Diagnostic Radiology",
        "5" ~ "Diagnostic Laboratory",
        "6" ~ "Therapeutic Radiology",
        "7" ~ "Anesthesia",
        "8" ~ "Assistant at Surgery",
        "9" ~ "Other Medical Items or Services",
        "0" ~ "Whole Blood Only",
        "A" ~ "Used DME",
        "B" ~ "High Risk Screening Mammography (exp 1998-01-01)",
        "C" ~ "Low Risk Screening Mammography (exp 1998-01-01)",
        "D" ~ "Ambulance",
        "E" ~ "Enteral/Parenteral Nutrients/Supplies",
        "F" ~ "Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "G" ~ "Immunosuppressive Drugs",
        "H" ~ "Hospice Services (exp 1995-01-01)",
        "I" ~ "Purchase of DME, Installment Basis (exp 1995-04-01)",
        "J" ~ "Diabetic Shoes",
        "K" ~ "Hearing Items and Services",
        "L" ~ "ESRD Supplies",
        "M" ~ "Monthly Capitation Payment for Dialysis",
        "N" ~ "Kidney Donor",
        "P" ~ "Lump Sum Purchase of DME, Prosthetics, Orthotics",
        "Q" ~ "Vision Items or Services",
        "R" ~ "Rental of DME",
        "S" ~ "Surgical Dressings or Other Medical Supplies",
        "T" ~ "Outpatient Mental Health Limitation",
        "U" ~ "Occupational Therapy",
        "V" ~ "Pneumococcal/Flu Vaccine",
        "W" ~ "Physical Therapy",
        "Y" ~ "Second Opinion on Elective Surgery (exp 1997-01-01)",
        "Z" ~ "Third Opinion on Elective Surgery (exp 1997-01-01)",
        "1:9" ~ "Medical Care, Other Medical Items or Services",
        "1:F" ~ "Medical Care, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "1:G:P" ~ "Medical Care, Immunosuppressive Drugs, Lump Sum Purchase of DME, Prosthetics, Orthotics",
        "1:L" ~ "Medical Care, ESRD Supplies",
        "1:L:P" ~ "Medical Care, ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics",
        "1:P" ~ "Medical Care, Lump Sum Purchase of DME, Prosthetics, Orthotics",
        "1:U:W" ~ "Medical Care, Occupational Therapy, Physical Therapy",
        "1:W" ~ "Medical Care, Physical Therapy",
        "2:9" ~ "Surgery, Other Medical Items or Services",
        "2:F" ~ "Surgery, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "4:F" ~ "Diagnostic Radiology, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "6:F" ~ "Therapeutic Radiology, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "9:F" ~ "Other Medical Items or Services, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "9:F:S" ~ "Other Medical Items or Services, Ambulatory Surgical Center (Facility Usage for Surgical Services), Surgical Dressings or Other Medical Supplies",
        "9:R" ~ "Other Medical Items or Services, Rental of DME",
        "9:S" ~ "Other Medical Items or Services, Surgical Dressings or Other Medical Supplies",
        "9:S:F" ~ "Other Medical Items or Services, Surgical Dressings or Other Medical Supplies, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
        "A:E:P:R" ~ "Used DME, Enteral/Parenteral Nutrients/Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
        "A:L:P:R" ~ "Used DME, ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
        "A:P:R" ~ "Used DME, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
        "L:P" ~ "ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics",
        "L:S" ~ "ESRD Supplies, Surgical Dressings or Other Medical Supplies",
        "P:R" ~ "Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
        "P:S" ~ "Lump Sum Purchase of DME, Prosthetics, Orthotics, Surgical Dressings or Other Medical Supplies"
      ),
      .after = {{ col }})
}
