#' Add Physician Supervision Descriptions
#'
#' This field is for use in post payment review.
#'
#' @param df data frame
#'
#' @param col column of Physician Supervision indicators
#'
#' @returns A [tibble][tibble::tibble-package] with a `supvis_description` column
#'
#' @examples
#' dplyr::tibble(supvis = stringr::str_pad(c(1:6, 9, 21:22, 66, "6A", 77, "7A"),
#'               width = "2", pad = "0")) |>
#' case_supervision(supvis)
#'
#' @autoglobal
#'
#' @export
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

#' Add OPPS Indicator Descriptions
#'
#' @param df data frame
#'
#' @param col column of OPPS Indicator indicators
#'
#' @returns A [tibble][tibble::tibble-package] with an `opps_description` column
#'
#' @examples
#' dplyr::tibble(opps_ind = c("1", "9")) |>
#' case_opps(opps_ind)
#'
#' @autoglobal
#'
#' @export
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
#'
#' @param col column of Modifier indicators
#'
#' @returns A [tibble][tibble::tibble-package] with an `mod_description` column
#'
#' @examples
#' dplyr::tibble(mod = c(26, "TC", 53)) |>
#' case_modifier(mod)
#'
#' @autoglobal
#'
#' @export
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
#' dplyr::tibble(pctc = as.character(0:9)) |>
#' case_pctc(pctc)
#' @export
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

#' Replace Status Codes with Labels, Add Descriptions
#'
#' @param df data frame
#' @param col column of Status Codes
#' @param desc add Descriptions
#' @return A [tibble][tibble::tibble-package] with a `status_desc` column
#' @examples
#' dplyr::tibble(status = LETTERS) |>
#' case_status(status, desc = TRUE)
#' @export
#' @autoglobal
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
#' @return A [tibble][tibble::tibble-package] with a `multiple_pricing_indicator` column
#' @examples
#' dplyr::tibble(mult_pi = c(9, LETTERS[1:7])) |> case_multiple_pricing(mult_pi)
#' @export
#' @autoglobal
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
#' @return A [tibble][tibble::tibble-package] with a `type_of_service` column
#' @examples
#' dplyr::tibble(tos = c(0:9, LETTERS)) |> case_tos(tos)
#' @export
#' @autoglobal
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
