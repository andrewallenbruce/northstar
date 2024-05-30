#' Global Days Descriptions
#'
#' @param x vector of Global Days indicators
#'
#' @returns vector of Global Days descriptions
#'
#' @examples
#' switch_global_days(c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_global_days <- function(x) {

  kit::nswitch(
    x = x,
    "000", "Endoscopic or minor procedure with related Preoperative and Postoperative RVUs on the day of the procedure only included in the fee schedule payment amount. E&M services on the day of the procedure generally not payable.",
    "010", "Minor procedure with Preoperative RVUs on the day of the procedure and Postoperative RVUs during a 10-day postoperative period included in the fee schedule amount. E&M services on the day of the procedure and during the 10-day postoperative period generally not payable.",
    "090", "Major surgery with a 1-day Preoperative period and 90-day Postoperative period included in fee schedule amount.",
    "MMM", "Maternity codes. Usual Global period does not apply.",
    "XXX", "Global concept does not apply.",
    "YYY", "Carrier determines if Global concept applies and, if appropriate, establishes Postoperative period.",
    "ZZZ", "Code related to another service and is always included in Global period of other service.",
    default = NA_character_
  )
}

#' Team Surgery Descriptions
#'
#' Modifier 66: Services for which Team Surgeons may be paid
#'
#' @param x vector of Team Surgery indicators
#'
#' @returns vector of Team Surgery descriptions
#'
#' @examples
#' switch_team_surgery(c(0:2, "9"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_team_surgery <- function(x) {

  kit::nswitch(
      x = x,
      "0", "Not Permitted",
      "1", "Medical Necessity Documentation Required",
      "2", "Permitted",
      "9", "Concept does not apply",
      default = NA_character_
  )
}

#' Bilateral Surgery Descriptions
#'
#' Modifier 50: Indicates services subject to payment adjustment
#'
#' @param x vector of Bilateral Surgery indicators
#'
#' @returns vector of Bilateral Surgery descriptions
#'
#' @examples
#' switch_bilateral_surgery(c(0:3, "9"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_bilateral_surgery <- function(x) {

  kit::nswitch(
    x = x,
    "0", "Adjustment does not apply. If reported with mod 50 or RT and LT, payment for the two sides is the lower of (a) total charge for both sides (b) 100% of fee schedule amount for a single code. Adjustment is inappropriate because (a) of physiology or anatomy, or (b) code description states it is a unilateral procedure and there is an existing code for the bilateral procedure.",
    "1", "Adjustment applies. If reported with bilateral modifier or twice on same day by any other means (with RT and LT mods, or with a 2 in the units field), base payment on lower of: (a) total charge for both sides or (b) 150% of fee schedule amount for a single code. If reported as bilateral procedure and reported with other procedure codes on same day, apply bilateral adjustment before applying any multiple procedure rules.",
    "2", "Adjustment does not apply. RVUs already based on procedure as a bilateral procedure. If reported with mod -50 or twice on same day by any other means, base payment on lower of (a) total charge for both sides, or (b) 100% of fee schedule for a single code.",
    "3", "Adjustment does not apply. If reported with mod 50 or for both sides on same day by any other means, base payment for each side or organ or site of paired organ on lower of (a) charge for each side or (b) 100% of fee schedule amount for each side. If reported as bilateral procedure and with other procedure codes on same day, determine fee schedule amount for a bilateral procedure before applying any multiple procedure rules. Services in this category are generally radiology procedures or other diagnostic tests which are not subject to the special payment rules for other bilateral surgeries.",
    "9", "Concept does not apply",
    default = NA_character_
  )
}

#' Multiple Procedure Descriptions
#'
#' Modifier 51: Indicates applicable payment adjustment rule for multiple
#' procedures
#'
#' @param x vector of Multiple Procedure indicators
#'
#' @returns vector of Multiple Procedure descriptions
#'
#' @examples
#' switch_multiple_procedure(as.character(0:9))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_multiple_procedure <- function(x) {

  kit::nswitch(
    x = x,
    "0", "No adjustment. If procedure is reported on the same day as another procedure, base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount for the procedure.",
    "1", "Standard adjustment. If reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 25%, 25%, 25%, and by report). Base payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
    "2", "Standard adjustment. If reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 50%, 50%, 50% and by report). Base payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
    "3", "Special rules for multiple endoscopic procedures apply if procedure is billed with another endoscopy in the same family (i.e., another endoscopy that has the same base procedure). The base procedure for each code with this indicator is identified in the Endobase column. Apply the multiple endoscopy rules to a family before ranking the family with the other procedures performed on the same day (for example, if multiple endoscopies in the same family are reported on the same day as endoscopies in another family or on the same day as a non-endoscopic procedure). If an endoscopic procedure is reported with only its base procedure, do not pay separately for the base procedure. Payment for the base procedure is included in the payment for the other endoscopy.",
    "4", "Special rules for the technical component (TC) of diagnostic imaging procedures apply if procedure is billed with another diagnostic imaging procedure in the same family (per the diagnostic imaging family indicator, below). If procedure is reported in the same session on the same day as another procedure with the same family indicator, rank the procedures by fee schedule amount for the TC. Pay 100% for the highest priced procedure, and 50% for each subsequent procedure. Base the payment for subsequent procedures on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage. Subject to 50% reduction of the TC diagnostic imaging (effective for services July 1, 2010 and after). Subject to 25% reduction of the PC of diagnostic imaging (effective for services January 1, 2012 through December 31, 2016). Subject to 5% reduction of the PC of diagnostic imaging (effective for services January 1, 2017 and after).",
    "5", "Subject to 50% of the practice expense component for certain therapy services.",
    "6", "Subject to 25% reduction of the second highest and subsequent procedures to the TC of diagnostic cardiovascular services, effective for services January 1, 2013, and thereafter.",
    "7", "Subject to 20% reduction of the second highest and subsequent procedures to the TC of diagnostic ophthalmology services, effective for services January 1, 2013, and thereafter.",
    "9", "Concept does not apply",
    default = NA_character_
  )
}

#' Co-Surgeon Descriptions
#'
#' Modifier 62: Services for which two surgeons,
#' each in a different specialty, may be paid.
#'
#' @param x vector of Co Surgeon indicators
#'
#' @returns vector of Co Surgeon descriptions
#'
#' @examples
#' switch_co_surgeon(c(0:2, 9, NA_character_))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_co_surgeon <- function(x) {

  kit::nswitch(
    x = x,
    "0", "Not Permitted",
    "1", "Medical Necessity Documentation Required",
    "2", "Permitted",
    "9", "Concept does not apply",
    default = NA_character_
    )
}

#' Assistant Surgery Descriptions
#'
#' Fee schedule amount equals 16 percent of amount
#' otherwise applicable for surgical payment.
#'
#' Modifiers:
#' * 80: Assistance by Another Physician
#' * 81: Minimal Assistance by a Another Physician
#' * 82: Assistance by Another Physician when Qualified Resident Surgeon Unavailable
#' * AS: Non-Physician Assistant at Surgery
#'
#' @param x vector of Assistant Surgery indicators
#'
#' @returns vector of Assistant Surgery descriptions
#'
#' @examples
#' switch_assistant_surgery(c(0:2, "9"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_assistant_surgery <- function(x) {

  kit::nswitch(
    x = x,
    "0", "Payment Restriction unless Medical Necessity documentation submitted",
    "1", "Payment Restriction; Assistant cannot be paid",
    "2", "No Payment Restriction; Assistant can be paid",
    "9", "Concept does not apply",
    default = NA_character_
  )
}

#' Diagnostic Imaging Descriptions
#'
#' Identifies the applicable Diagnostic Service family for
#' HCPCS codes with a Multiple Procedure indicator of 4.
#'
#' @param x vector of Diagnostic Imaging indicators
#'
#' @returns vector of Diagnostic Imaging descriptions
#'
#' @examples
#' switch_diagnostic_imaging(fuimus::pad_number(c(1:11, 88, 99)))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_diagnostic_imaging <- function(x) {

  kit::nswitch(
    x = x,
    "01", "Ultrasound (Chest / Abdomen / Pelvis-Non-Obstetrical)",
    "02", "CT and CTA (Chest / Thorax / Abd / Pelvis)",
    "03", "CT and CTA (Head / Brain / Orbit / Maxillofacial / Neck)",
    "04", "MRI and MRA (Chest / Abd / Pelvis)",
    "05", "MRI and MRA (Head / Brain / Neck)",
    "06", "MRI and MRA (Spine)",
    "07", "CT (Spine)",
    "08", "MRI and MRA (Lower Extremities)",
    "09", "CT and CTA (Lower Extremities)",
    "10", "MRI and MRA (Upper Extremities and Joints)",
    "11", "CT and CTA (Upper Extremities)",
    "88", "Subject to Reduction of TC or PC Diagnostic Imaging",
    "99", "Concept does not apply",
    default = NA_character_
    )
}

