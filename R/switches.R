#' NCCI PTP CCMI Descriptions
#'
#' @param x vector of PTP CCMIs
#'
#' @returns vector of PTP CCMI descriptions
#'
#' @examples
#' switch_ptp_mod(as.character(0:9))
#'
#' @autoglobal
#'
#' @export
switch_ptp_mod <- function(x) {

  kit::nswitch(
    x = x,
    "0", "Not Allowed",
    "1", "Allowed",
    "9", "Not Applicable",
    default = NA_character_
  )
}

#' NCCI MUE Adjudication Indicator Descriptions
#'
#' @param x vector of MUE Adjudication Indicators
#'
#' @returns vector of MUE Adjudication Indicator descriptions
#'
#' @examples
#' switch_mue_mai(as.character(1:3))
#'
#' @autoglobal
#'
#' @export
switch_mue_mai <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Line Edit",
    "2", "Date of Service Edit: Policy",
    "3", "Date of Service Edit: Clinical",
    default = NA_character_
  )
}

#' NCCI Add-On Code Edit Type Descriptions
#'
#' @param x vector of Add-On Code Edit Types
#'
#' @returns vector of Add-On Code Edit Type descriptions
#'
#' @examples
#' switch_addon_edits(as.character(1:3))
#'
#' @autoglobal
#'
#' @export
switch_addon_edits <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Only Paid if Primary is Paid. Payment Eligible if Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.",
    "2", "No Specific Primary Codes. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.",
    "3", "Some Specific Primaries. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.",
    default = NA_character_
  )
}

#' PFS Global Days Descriptions
#'
#' @param x vector of Global Days indicators
#'
#' @returns vector of Global Days descriptions
#'
#' @examples
#' x <- c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ")
#' switch_global_days(x)
#'
#' @autoglobal
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

#' PFS Team Surgery Descriptions
#'
#' Modifier 66: Services for which Team Surgeons may be paid
#'
#' @param x vector of Team Surgery indicators
#'
#' @returns vector of Team Surgery descriptions
#'
#' @examples
#' switch_team_surgery(as.character(0:9))
#'
#' @autoglobal
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

#' PFS Bilateral Surgery Descriptions
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

#' PFS Multiple Procedure Descriptions
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

#' PFS Co-Surgeon Descriptions
#'
#' Modifier 62: Services for which two surgeons,
#' each in a different specialty, may be paid.
#'
#' @param x vector of Co-Surgeon indicators
#'
#' @returns vector of Co-Surgeon descriptions
#'
#' @examples
#' switch_co_surgeon(as.character(0:9))
#'
#' @autoglobal
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

#' PFS Assistant Surgery Descriptions
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
#' switch_assistant_surgery(as.character(0:9))
#'
#' @autoglobal
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

#' PFS Diagnostic Imaging Descriptions
#'
#' Identifies the applicable Diagnostic Service family for
#' HCPCS codes with a Multiple Procedure indicator of 4.
#'
#' @param x vector of Diagnostic Imaging indicators
#'
#' @returns vector of Diagnostic Imaging descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = fuimus::pad_number(c(1:11, 88, 99)),
#'   desc = switch_diagnostic_imaging(code))
#'
#' @autoglobal
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

#' PFS OPPS Indicator Descriptions
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
#' @export
switch_opps_indicator <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Subject to OPPS Payment Cap",
    "9", "Not Subject to OPPS Payment Cap",
    default = NA_character_
  )
}

#' PFS PC/TC Indicator Descriptions
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

#' PFS Status Code Names
#'
#' @param x vector of status codes
#'
#' @returns vector of status code names
#'
#' @examples
#' dplyr::tibble(
#'   code = LETTERS,
#'   name = switch_status_name(LETTERS),
#'   desc = switch_status_description(LETTERS)
#' )
#'
#' @autoglobal
#'
#' @export
switch_status_name <- function(x) {

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
}

#' PFS Status Code Descriptions
#'
#' @param x vector of status codes
#'
#' @returns vector of status code descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = LETTERS,
#'   name = switch_status_name(LETTERS),
#'   desc = switch_status_description(LETTERS)
#' )
#'
#' @autoglobal
#'
#' @export
switch_status_description <- function(x) {

  kit::nswitch(
    x = x,
    "A", "Separately paid if covered. RVUs and payment amounts. Carriers responsible for coverage decisions in absence of an NCD.",
    "B", "Payment bundled into payment for other services not specified. No RVUs, no payment made. When covered, payment subsumed by payment for services to which they are incident.",
    "C", "Carriers establish RVUs and payment following documentation review.",
    "D", "Deleted effective with beginning of year.",
    "E", "Excluded by regulation. No RVUs, no payment made. When covered, payment made under reasonable charge procedures.",
    "F", "Not subject to 90 day grace period",
    "G", "Another code used for payment. Subject to a 90 day grace period.",
    "H", "Had TC/26 mod in previous year, TC/26 component now deleted.",
    "I", "Another code used for payment. Not subject to a 90-day grace period.",
    "J", "No RVUs or payment amounts. Only identifies anesthesia services.",
    "M", "Used for reporting purposes only.",
    "N", "Not covered by Medicare.",
    "P", "No RVUs, no payment made. If covered as Incident To and provided on same day as physician service, payment bundled into payment for Incident To service. If covered as other than Incident To, paid under other payment provision.",
    "R", "Special coverage instructions apply. If covered, service is contractor priced. Assigned to limited number of codes covered in unusual circumstances. Majority of codes are dental codes.",
    "T", "RVUs and payment amounts. Paid only if no other payable services billed on same date by same provider. If payable services billed, bundled into payment.",
    "X", "Not in statutory definition of Physician Services. No RVUs or payment amounts, no payment made.",
    default = NA_character_
  )
}

#' HCPCS Physician Supervision Descriptions
#'
#' This field is for use in post payment review.
#'
#' @param x vector of Physician Supervision indicators
#'
#' @returns vector of Physician Supervision descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c(fuimus::pad_number(
#'   c(1:6, 9, 21:22, 66, 77)), "6A", "7A"),
#'   desc = switch_supervision(code))
#'
#' @autoglobal
#'
#' @export
switch_supervision <- function(x) {

  kit::nswitch(
    x = x,
    "01", "Must be performed under General supervision",
    "02", "Must be performed under Direct supervision",
    "03", "Must be performed under Personal supervision",
    "04", "Must be performed under General supervision unless performed by Qualified, Independent Psychologist or Clinical Psychologist",
    "05", "Must be performed under General supervision unless performed by Qualified Audiologist",
    "06", "Must be performed by Physician or Qualified Electrophysiological Clinical Specialist (ABPTS-certified) permitted to provide procedure under State law",
    "21", "May be performed by Certified Technician under General supervision; otherwise, Direct supervision",
    "22", "May be performed by Technician with online real-time Physician contact",
    "66", "May be performed by Physician or ABPTS-certified physical therapist with certification in procedure",
    "6A", "In addition to Level 66 rule, ABPTS-certified PT may supervise another PT, but only ABPTS-certified PT may bill",
    "77", "Must be performed by either ABPTS-certified PT, Uncertified PT under Direct supervision, or Certified Technician under General supervision",
    "7A", "In addition to Level 77 rule, ABPTS-certified PT may supervise another PT, but only ABPTS-certified PT may bill",
    "09", "Concept does not apply",
    default = NA_character_
  )
}

#' HCPCS ASC Group Descriptions
#'
#' @param x vector of ASC Group indicators
#'
#' @returns vector of ASC Group descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c("YY", 99),
#'   desc = switch_asc_group(code))
#'
#' @autoglobal
#'
#' @export
switch_asc_group <- function(x) {

  kit::nswitch(
    x = x,
    "YY", "Approved For ASCs",
    "99", "Not Approved For ASCs",
    default = NA_character_
  )
}

#' HCPCS Coverage Descriptions
#'
#' @param x vector of Coverage indicators
#'
#' @returns vector of Coverage descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c("C", "D", "I", "M", "S"),
#'   desc = switch_coverage(code))
#'
#' @autoglobal
#'
#' @export
switch_coverage <- function(x) {

  kit::nswitch(
    x = x,
    "C", "Carrier Judgment",
    "D", "Special Coverage Instructions Apply",
    "I", "Not Payable by Medicare",
    "M", "Not Covered by Medicare",
    "S", "Not Covered by Medicare Statute",
    default = NA_character_
  )
}

#' HCPCS Pricing Descriptions
#'
#' @param x vector of Pricing indicators
#'
#' @returns vector of Pricing descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c("00", 11:13, 21:22, 31:46, 51:57, 99),
#'   desc = switch_pricing(code))
#'
#' @autoglobal
#'
#' @export
switch_pricing <- function(x) {

  kit::nswitch(
    x = x,
    "00", "Not Separately Priced by Part B",
    "11", "Priced with National RVUs",
    "12", "Priced with National Anesthesia Base Units",
    "13", "Carrier Priced",
    "22", "Carrier Priced",
    "46", "Carrier Priced",
    "57", "Carrier Priced",
    "21", "Price Subject to National Limitation Amount",
    "31", "Frequently Serviced DME (Subject to Floors & Ceilings)",
    "32", "Inexpensive & Routinely Purchased DME (Subject to Floors & Ceilings)",
    "33", "Oxygen & Oxygen Equipment (Subject to Floors & Ceilings)",
    "34", "DME Supplies (Subject to Floors & Ceilings)",
    "35", "Surgical dressings (Subject to Floors & Ceilings)",
    "36", "Capped Rental DME (Subject to Floors & Ceilings)",
    "37", "Ostomy, Tracheostomy & Urological Supplies (Subject to Floors & Ceilings)",
    "38", "Orthotics, prosthetics, prosthetic devices & vision services (Subject to Floors & Ceilings)",
    "39", "Parenteral & Enteral Nutrition",
    "40", "Lymphedema Compression Treatment Items",
    "45", "Customized DME Items",
    "51", "Drugs",
    "52", "Reasonable Charge",
    "53", "Statute",
    "54", "Vaccinations",
    "55", "Splints & Casts",
    "56", "IOLs Inserted in Physician Office",
    "99", "Value Not Established",
    "11:21", "Priced with National RVUs, Subject to National Limitation Amount",
    "13:21", "Carrier Priced, Subject to National Limitation Amount",
    "57:21", "Carrier Priced, Subject to National Limitation Amount",
    default = NA_character_
  )
}

#' HCPCS Multiple Pricing Descriptions
#'
#' @param x vector of Multiple Pricing indicators
#'
#' @returns vector of Multiple Pricing descriptions
#'
#' @examples
#' dplyr::tibble(code = c(9, LETTERS[1:7]))
#'
#' @autoglobal
#'
#' @export
switch_multiple_pricing <- function(x) {

  kit::nswitch(
    x = x,
    "9", "Not Applicable. Not Priced Separately by Part B or Value Not Established.",
    "A", "Not Applicable. Priced Under One Methodology.",
    "B", "Professional Component Priced with RVUs. Technical Component & Global Service Priced by Part B Carriers.",
    "C", "Physician Interpretation Priced with PFS RVUs. Lab Service Paid under Clinical Lab Fee Schedule.",
    "D", "Not Applicable as of 1998-01-01.",
    "E", "Not Applicable as of 1998-01-01.",
    "F", "Not Applicable as of 1998-01-01.",
    "G", "If Submitted on Claim with Blood Products, Priced Under Reasonable Charge. If No Blood Products, Priced Under Clinical Lab Fee Schedule.",
    default = NA_character_
  )
}

#' HCPCS Lab Certification Descriptions
#'
#' @param x vector of Lab Certification indicators
#'
#' @returns vector of Lab Certification descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c("010", 100, 110, 115, 120, "110, 120, 130, 400"),
#'   desc = switch_lab_certification(code))
#'
#' @autoglobal
#'
#' @export
switch_lab_certification <- function(x) {

  kit::nswitch(
    x = x,
    "010", "Histocompatibility Testing",
    "100", "Microbiology",
    "110", "Bacteriology",
    "115", "Mycobacteriology",
    "120", "Mycology",
    "130", "Parasitology",
    "140", "Virology",
    "150", "Other Microbiology",
    "200", "Diagnostic Immunology",
    "210", "Syphilis Serology",
    "220", "General Immunology",
    "300", "Chemistry",
    "310", "Routine Chemistry",
    "320", "Urinalysis",
    "330", "Endocrinology",
    "340", "Toxicology",
    "350", "Other Chemistry",
    "400", "Hematology",
    "500", "Immunohematology",
    "510", "ABO Group & RH Type",
    "520", "Antibody Detection (Transfusion)",
    "530", "Antibody Detection (Nontransfusion)",
    "540", "Antibody Identification",
    "550", "Compatibility Testing",
    "560", "Other Immunohematology",
    "600", "Pathology",
    "610", "Histopathology",
    "620", "Oral Pathology",
    "630", "Cytology",
    "800", "Radiobioassay",
    "900", "Clinical Cytogenetics",
    "140, 220", "Virology, General Immunology",
    "220, 400", "General Immunology, Hematology",
    "220, 610", "General Immunology, Histopathology",
    "310, 400", "Routine Chemistry, Hematology",
    "220, 310, 900", "General Immunology, Routine Chemistry, Clinical Cytogenetics",
    "110, 120, 130, 400", "Bacteriology, Mycology, Parasitology, Hematology",
    default = NA_character_
  )
}

#' HCPCS Type of Service Descriptions
#'
#' @param x vector of Type of Service codes
#'
#' @returns vector of Type of Service descriptions
#'
#' @examples
#' dplyr::tibble(code = c(0:9, LETTERS),
#'               desc = switch_type_of_service(code))
#'
#' @autoglobal
#'
#' @export
switch_type_of_service <- function(x) {

  kit::nswitch(
    x = x,
    "1", "Medical Care",
    "2", "Surgery",
    "3", "Consultation",
    "4", "Diagnostic Radiology",
    "5", "Diagnostic Laboratory",
    "6", "Therapeutic Radiology",
    "7", "Anesthesia",
    "8", "Assistant at Surgery",
    "9", "Other Medical Items or Services",
    "0", "Whole Blood Only",
    "A", "Used DME",
    "B", "High Risk Screening Mammography (exp 1998-01-01)",
    "C", "Low Risk Screening Mammography (exp 1998-01-01)",
    "D", "Ambulance",
    "E", "Enteral/Parenteral Nutrients/Supplies",
    "F", "Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "G", "Immunosuppressive Drugs",
    "H", "Hospice Services (exp 1995-01-01)",
    "I", "Purchase of DME, Installment Basis (exp 1995-04-01)",
    "J", "Diabetic Shoes",
    "K", "Hearing Items and Services",
    "L", "ESRD Supplies",
    "M", "Monthly Capitation Payment for Dialysis",
    "N", "Kidney Donor",
    "P", "Lump Sum Purchase of DME, Prosthetics, Orthotics",
    "Q", "Vision Items or Services",
    "R", "Rental of DME",
    "S", "Surgical Dressings or Other Medical Supplies",
    "T", "Outpatient Mental Health Limitation",
    "U", "Occupational Therapy",
    "V", "Pneumococcal/Flu Vaccine",
    "W", "Physical Therapy",
    "Y", "Second Opinion on Elective Surgery (exp 1997-01-01)",
    "Z", "Third Opinion on Elective Surgery (exp 1997-01-01)",
    "1:9", "Medical Care, Other Medical Items or Services",
    "1:F", "Medical Care, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "1:L", "Medical Care, ESRD Supplies",
    "1:P", "Medical Care, Lump Sum Purchase of DME, Prosthetics, Orthotics",
    "1:W", "Medical Care, Physical Therapy",
    "2:9", "Surgery, Other Medical Items or Services",
    "2:F", "Surgery, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "4:F", "Diagnostic Radiology, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "6:F", "Therapeutic Radiology, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "9:F", "Other Medical Items or Services, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "9:R", "Other Medical Items or Services, Rental of DME",
    "9:S", "Other Medical Items or Services, Surgical Dressings or Other Medical Supplies",
    "L:S", "ESRD Supplies, Surgical Dressings or Other Medical Supplies",
    "P:R", "Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
    "P:S", "Lump Sum Purchase of DME, Prosthetics, Orthotics, Surgical Dressings or Other Medical Supplies",
    "L:P", "ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics",
    "1:G:P", "Medical Care, Immunosuppressive Drugs, Lump Sum Purchase of DME, Prosthetics, Orthotics",
    "1:L:P", "Medical Care, ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics",
    "1:U:W", "Medical Care, Occupational Therapy, Physical Therapy",
    "9:F:S", "Other Medical Items or Services, Ambulatory Surgical Center (Facility Usage for Surgical Services), Surgical Dressings or Other Medical Supplies",
    "9:S:F", "Other Medical Items or Services, Surgical Dressings or Other Medical Supplies, Ambulatory Surgical Center (Facility Usage for Surgical Services)",
    "A:P:R", "Used DME, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
    "A:E:P:R", "Used DME, Enteral/Parenteral Nutrients/Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
    "A:L:P:R", "Used DME, ESRD Supplies, Lump Sum Purchase of DME, Prosthetics, Orthotics, Rental of DME",
    default = NA_character_
  )
}

#' HCPCS Action Descriptions
#'
#' @param x vector of Action codes
#'
#' @returns vector of Action descriptions
#'
#' @examples
#' dplyr::tibble(code = c(LETTERS),
#'               desc = switch_action_code(code))
#'
#' @autoglobal
#'
#' @export
switch_action_code <- function(x) {

  kit::nswitch(
    x = x,
    "A", "Added procedure or modifier code",
    "B", "Change in both administrative data field and long description of procedure or modifier code",
    "C", "Change in long description of procedure or modifier code",
    "D", "Discontinue procedure or modifier code",
    "F", "Change in administrative data field of procedure or modifier code",
    "N", "No maintenance for this code",
    "P", "Payment change (MOG, pricing indicator codes, anesthesia base units,Ambulatory Surgical Centers)",
    "R", "Re-activate discontinued/deleted procedure or modifier code",
    "S", "Change in short description of procedure code",
    "T", "Miscellaneous change (BETOS, type of service)",
    default = NA_character_
  )
}

#' HCPCS BETOS Descriptions
#'
#' @param x vector of BETOS codes
#'
#' @returns vector of BETOS descriptions
#'
#' @examples
#' dplyr::tibble(code = c("D1A", "I2A", "M3", "P0"),
#'               desc = switch_betos(code))
#'
#' @autoglobal
#'
#' @export
switch_betos <- function(x) {

  kit::nswitch(
    x = x,
    "D1A", "Medical/Surgical Supplies",
    "D1B", "Hospital Seds",
    "D1C", "Oxygen & Supplies",
    "D1D", "Wheelchairs",
    "D1E", "Other DME",
    "D1F", "Prosthetic/Orthotic Devices",
    "D1G", "DME Administered Drugs",
    "I1A", "Standard Imaging: Chest",
    "I1B", "Standard Imaging: Musculoskeletal",
    "I1C", "Standard Imaging: Breast",
    "I1D", "Standard Imaging: Contrast GI",
    "I1E", "Standard Imaging: Nuclear Medicine",
    "I1F", "Standard Imaging: Other",
    "I2A", "Advanced Imaging: CAT-CT-CTA, Brain-Head-Neck",
    "I2B", "Advanced Imaging: CAT-CT-CTA, Other",
    "I2C", "Advanced Imaging: MRI-MRA, Brain-Head-Neck",
    "I2D", "Advanced Imaging: MRI-MRA, Other",
    "I3A", "Echography-Ultrasonography: Eye",
    "I3B", "Echography-Ultrasonography: Abdomen-Pelvis",
    "I3C", "Echography-Ultrasonography: Heart",
    "I3D", "Echography-Ultrasonography: Carotid Arteries",
    "I3E", "Echography-Ultrasonography: Prostate, Transrectal",
    "I3F", "Echography-Ultrasonography: Other",
    "I4A", "Imaging-Procedure: Heart incl Cardiac Cath",
    "I4B", "Imaging-Procedure: Other",
    "M1A", "Office Visit: New",
    "M1B", "Office Visit: Established",
    "M2A", "Hospital Visit: Initial",
    "M2B", "Hospital Visit: Subsequent",
    "M2C", "Hospital Visit: Critical Care",
    "M3" , "ER Visit",
    "M4A", "Home Visit",
    "M4B", "Nursing Home Visit",
    "M5A", "Specialist: Pathology",
    "M5B", "Specialist: Psychiatry",
    "M5C", "Specialist: Opthamology",
    "M5D", "Specialist: Other",
    "M6" , "Consultations",
    "O1A", "Ambulance",
    "O1B", "Chiropractic",
    "O1C", "Enteral & Parenteral",
    "O1D", "Chemotherapy",
    "O1E", "Other Drugs",
    "O1F", "Hearing & Speech Services",
    "O1G", "Immunizations-Vaccinations",
    "01L", "Lymphedema Compression Treatment Items",
    "P0" , "Anesthesia",
    "P1A", "Major Procedure: Breast",
    "P1B", "Major Procedure: Colectomy",
    "P1C", "Major Procedure: Cholecystectomy",
    "P1D", "Major Procedure: TURP",
    "P1E", "Major Procedure: Hysterectomy",
    "P1F", "Major Procedure: Explor-Decompr-Excisdisc",
    "P1G", "Major Procedure: Other",
    "P2A", "Major Procedure: Cardiovascular - CABG",
    "P2B", "Major Procedure: Cardiovascular - Aneurysm Repair",
    "P2C", "Major Procedure: Cardiovascular - Thromboendarterectomy",
    "P2D", "Major Procedure: Cardiovascular - Coronary angioplasty (PTCA)",
    "P2E", "Major Procedure: Cardiovascular - Pacemaker Insertion",
    "P2F", "Major Procedure: Cardiovascular - Other",
    "P3A", "Major Procedure: Orthopedic - Hip Fracture Repair",
    "P3B", "Major Procedure: Orthopedic - Hip Replacement",
    "P3C", "Major Procedure: Orthopedic - Knee Replacement",
    "P3D", "Major Procedure: Orthopedic - Other",
    "P4A", "Eye procedure: Corneal Transplant",
    "P4B", "Eye procedure: Cataract Removal/Lens Insertion",
    "P4C", "Eye procedure: Retinal Detachment",
    "P4D", "Eye procedure: Treatment of Retinal Lesions",
    "P4E", "Eye procedure: Other",
    "P5A", "Ambulatory procedures: Skin",
    "P5B", "Ambulatory procedures: Musculoskeletal",
    "P5C", "Ambulatory procedures: Inguinal Hernia Repair",
    "P5D", "Ambulatory procedures: Lithotripsy",
    "P5E", "Ambulatory procedures: Other",
    "P6A", "Minor procedures: Skin",
    "P6B", "Minor procedures: Musculoskeletal",
    "P6C", "Minor procedures: Other (Medicare Fee Schedule)",
    "P6D", "Minor procedures: Other (Non-Medicare Fee Schedule)",
    "P7A", "Oncology: Radiation Therapy",
    "P7B", "Oncology: Other",
    "P8A", "Endoscopy: Arthroscopy",
    "P8B", "Endoscopy: Upper GI",
    "P8C", "Endoscopy: Sigmoidoscopy",
    "P8D", "Endoscopy: Colonoscopy",
    "P8E", "Endoscopy: Cystoscopy",
    "P8F", "Endoscopy: Bronchoscopy",
    "P8G", "Endoscopy: Laparoscopic Cholecystectomy",
    "P8H", "Endoscopy: Laryngoscopy",
    "P8I", "Endoscopy: Other",
    "P9A", "Dialysis Services (Medicare Fee Schedule)",
    "P9B", "Dialysis Services (Non-Medicare Fee Schedule)",
    "T1A", "Lab tests: Routine Venipuncture (Non-Medicare Fee Schedule)",
    "T1B", "Lab tests: Automated General Profiles",
    "T1C", "Lab tests: Urinalysis",
    "T1D", "Lab tests: Blood Counts",
    "T1E", "Lab tests: Glucose",
    "T1F", "Lab tests: Bacterial Cultures",
    "T1G", "Lab tests: Other (Medicare Fee Schedule)",
    "T1H", "Lab tests: Other (Non-Medicare Fee Schedule)",
    "T2A", "Other tests: ECGs",
    "T2B", "Other tests: Cardiovascular Stress Tests",
    "T2C", "Other tests: EKG Monitoring",
    "T2D", "Other tests: Other",
    "Y1", "Other: Medicare Fee Schedule",
    "Y2", "Other: Non-Medicare Fee Schedule",
    "Z1", "Local Codes",
    "Z2", "Undefined Codes",
    default = NA_character_
  )
}
