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
#' @param x vector of Co-Surgeon indicators
#'
#' @returns vector of Co-Surgeon descriptions
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

#' OPPS Indicator Descriptions
#'
#' @param x vector of OPPS indicators
#'
#' @returns vector of OPPS indicator descriptions
#'
#' @examples
#' switch_opps_indicator(c("1", "9"))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_opps_indicator <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Subject to OPPS Payment Cap",
    "9", "Not Subject to OPPS Payment Cap",
    default = NA_character_
  )
}

#' PC/TC Indicator Descriptions
#'
#' @param x vector of PC/TC indicators
#'
#' @returns vector of PC/TC indicator descriptions
#'
#' @examples
#' switch_pctc_indicator(as.character(0:9))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_pctc_indicator <- function(x) {

  kit::nswitch(
    x = x,
    "0", "Physician Service: PC/TC does not apply",
    "1", "Diagnostic Tests for Radiology Services: Have both a PC and TC. Mods 26/TC can be used.",
    "2", "Professional Component Only: Standalone code. Describes PC of diagnostic tests for which there is a code that describes TC of diagnostic test only and another code that describes the Global test.",
    "3", "Technical Component Only: Standalone code. Mods 26/TC cannot be used. Describe TC of diagnostic tests for which there is a code that describes PC of the diagnostic test only. Also identifies codes that are covered only as diagnostic tests and do not have a PC code.",
    "4", "Global Test Only: Standalone code. Mods 26/TC cannot be used. Describes diagnostic tests for which there are codes that describe PC of the test only, and the TC of the test only. Total RVUs is sum of total RVUs for PC and TC only codes combined.",
    "5", "Incident-To:  Mods 26/TC cannot be used. Services provided by personnel working under physician supervision. Payment may not be made when provided to hospital inpatients or outpatients.",
    "6", "Lab Physician Interpretation: Mod TC cannot be used. Clinical Lab codes for which separate payment for interpretations by laboratory physicians may be made. Actual performance of tests paid by lab fee schedule.",
    "7", "Physical Therapy: Payment may not be made if provided to hospital outpatient/inpatient by independently practicing physical or occupational therapist.",
    "8", "Physician Interpretation: Identifies PC of Clinical Lab codes for which separate payment made only if physician interprets abnormal smear for hospital inpatient. No TC billing recognized, payment for test made to hospital. No payment for CPT 85060 furnished to hospital outpatients or non-hospital patients. Physician interpretation paid through clinical laboratory fee schedule.",
    "9", "PCTC Concept does not apply",
    default = NA_character_
  )
}

#' Status Code Descriptions
#'
#' @param x vector of PC/TC indicators
#'
#' @returns vector of PC/TC indicator descriptions
#'
#' @examples
#' switch_status_code(LETTERS)
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
switch_status_code <- function(x) {

  kit::nswitch(
    x = x,
    "A", "Active",
    "B", "Payment Bundle",
    "C", "Carrier Priced",
    "D", "Deleted Codes",
    "E", "Regulatory Exclusion",
    "F", "Deleted/Discontinued",
    "G", "Not Valid for Medicare",
    "H", "Deleted Modifier",
    "I", "Not Valid for Medicare",
    "J", "Anesthesia Service",
    "M", "Measurement Code",
    "N", "Restricted Coverage",
    "P", "Non-Covered Service",
    "R", "Bundled/Excluded Code",
    "T", "Injections",
    "X", "Statutory Exclusion",
    default = NA_character_
  )

  # kit::nswitch(
  #   x = x,
  #   "A", "Separately paid if covered. RVUs and payment amounts. Carriers responsible for coverage decisions in absence of an NCD.",
  #   "B", "Payment bundled into payment for other services not specified. No RVUs, no payment made. When covered, payment subsumed by payment for services to which they are incident.",
  #   "C", "Carriers establish RVUs and payment following documentation review.",
  #   "D", "Deleted effective with beginning of year.",
  #   "E", "Excluded by regulation. No RVUs, no payment made. When covered, payment made under reasonable charge procedures.",
  #   "F", "Not subject to 90 day grace period",
  #   "G", "Another code used for payment. Subject to a 90 day grace period.",
  #   "H", "Had TC/26 mod in previous year, TC/26 component now deleted.",
  #   "I", "Another code used for payment. Not subject to a 90-day grace period.",
  #   "J", "No RVUs or payment amounts. Only identifies anesthesia services.",
  #   "M", "Used for reporting purposes only.",
  #   "N", "Not covered by Medicare.",
  #   "P", "No RVUs, no payment made. If covered as Incident To and provided on same day as physician service, payment bundled into payment for Incident To service. If covered as other than Incident To, paid under other payment provision.",
  #   "R", "Special coverage instructions apply. If covered, service is contractor priced. Assigned to limited number of codes covered in unusual circumstances. Majority of codes are dental codes.",
  #   "T", "RVUs and payment amounts. Paid only if no other payable services billed on same date by same provider. If payable services billed, bundled into payment.",
  #   "X", "Not in statutory definition of Physician Services. No RVUs or payment amounts, no payment made.",
  #   default = NA_character_
  # )

}
