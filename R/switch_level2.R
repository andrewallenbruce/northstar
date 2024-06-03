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

#' @autoglobal
#' @noRd
action_cd <- function() {
  c(
    "A" = "Added procedure or modifier code",
    "B" = "Change in both administrative data field and long description of procedure or modifier code",
    "C" = "Change in long description of procedure or modifier code",
    "D" = "Discontinue procedure or modifier code",
    "F" = "Change in administrative data field of procedure or modifier code",
    "N" = "No maintenance for this code",
    "P" = "Payment change (MOG, pricing indicator codes, anesthesia base units,Ambulatory Surgical Centers)",
    "R" = "Re-activate discontinued/deleted procedure or modifier code",
    "S" = "Change in short description of procedure code",
    "T" = "Miscellaneous change (BETOS, type of service)"
  )
}

#' @autoglobal
#' @noRd
betos <- function() {
  c(
    "D1A" = "Medical/Surgical Supplies",
    "D1B" = "Hospital Seds",
    "D1C" = "Oxygen & Supplies",
    "D1D" = "Wheelchairs",
    "D1E" = "Other DME",
    "D1F" = "Prosthetic/Orthotic Devices",
    "D1G" = "DME Administered Drugs",
    "I1A" = "Standard Imaging: Chest",
    "I1B" = "Standard Imaging: Musculoskeletal",
    "I1C" = "Standard Imaging: Breast",
    "I1D" = "Standard Imaging: Contrast GI",
    "I1E" = "Standard Imaging: Nuclear Medicine",
    "I1F" = "Standard Imaging: Other",
    "I2A" = "Advanced Imaging: CAT-CT-CTA, Brain-Head-Neck",
    "I2B" = "Advanced Imaging: CAT-CT-CTA, Other",
    "I2C" = "Advanced Imaging: MRI-MRA, Brain-Head-Neck",
    "I2D" = "Advanced Imaging: MRI-MRA, Other",
    "I3A" = "Echography-Ultrasonography: Eye",
    "I3B" = "Echography-Ultrasonography: Abdomen-Pelvis",
    "I3C" = "Echography-Ultrasonography: Heart",
    "I3D" = "Echography-Ultrasonography: Carotid Arteries",
    "I3E" = "Echography-Ultrasonography: Prostate, Transrectal",
    "I3F" = "Echography-Ultrasonography: Other",
    "I4A" = "Imaging-Procedure: Heart incl Cardiac Cath",
    "I4B" = "Imaging-Procedure: Other",
    "M1A" = "Office Visit: New",
    "M1B" = "Office Visit: Established",
    "M2A" = "Hospital Visit: Initial",
    "M2B" = "Hospital Visit: Subsequent",
    "M2C" = "Hospital Visit: Critical Care",
    "M3"  = "ER Visit",
    "M4A" = "Home Visit",
    "M4B" = "Nursing Home Visit",
    "M5A" = "Specialist: Pathology",
    "M5B" = "Specialist: Psychiatry",
    "M5C" = "Specialist: Opthamology",
    "M5D" = "Specialist: Other",
    "M6"  = "Consultations",
    "O1A" = "Ambulance",
    "O1B" = "Chiropractic",
    "O1C" = "Enteral & Parenteral",
    "O1D" = "Chemotherapy",
    "O1E" = "Other Drugs",
    "O1F" = "Hearing & Speech Services",
    "O1G" = "Immunizations-Vaccinations",
    "01L" = "Lymphedema Compression Treatment Items",
    "P0"  = "Anesthesia",
    "P1A" = "Major Procedure: Breast",
    "P1B" = "Major Procedure: Colectomy",
    "P1C" = "Major Procedure: Cholecystectomy",
    "P1D" = "Major Procedure: TURP",
    "P1E" = "Major Procedure: Hysterectomy",
    "P1F" = "Major Procedure: Explor-Decompr-Excisdisc",
    "P1G" = "Major Procedure: Other",
    "P2A" = "Major Procedure: Cardiovascular - CABG",
    "P2B" = "Major Procedure: Cardiovascular - Aneurysm Repair",
    "P2C" = "Major Procedure: Cardiovascular - Thromboendarterectomy",
    "P2D" = "Major Procedure: Cardiovascular - Coronary angioplasty (PTCA)",
    "P2E" = "Major Procedure: Cardiovascular - Pacemaker Insertion",
    "P2F" = "Major Procedure: Cardiovascular - Other",
    "P3A" = "Major Procedure: Orthopedic - Hip Fracture Repair",
    "P3B" = "Major Procedure: Orthopedic - Hip Replacement",
    "P3C" = "Major Procedure: Orthopedic - Knee Replacement",
    "P3D" = "Major Procedure: Orthopedic - Other",
    "P4A" = "Eye procedure: Corneal Transplant",
    "P4B" = "Eye procedure: Cataract Removal/Lens Insertion",
    "P4C" = "Eye procedure: Retinal Detachment",
    "P4D" = "Eye procedure: Treatment of Retinal Lesions",
    "P4E" = "Eye procedure: Other",
    "P5A" = "Ambulatory procedures: Skin",
    "P5B" = "Ambulatory procedures: Musculoskeletal",
    "P5C" = "Ambulatory procedures: Inguinal Hernia Repair",
    "P5D" = "Ambulatory procedures: Lithotripsy",
    "P5E" = "Ambulatory procedures: Other",
    "P6A" = "Minor procedures: Skin",
    "P6B" = "Minor procedures: Musculoskeletal",
    "P6C" = "Minor procedures: Other (Medicare Fee Schedule)",
    "P6D" = "Minor procedures: Other (Non-Medicare Fee Schedule)",
    "P7A" = "Oncology: Radiation Therapy",
    "P7B" = "Oncology: Other",
    "P8A" = "Endoscopy: Arthroscopy",
    "P8B" = "Endoscopy: Upper GI",
    "P8C" = "Endoscopy: Sigmoidoscopy",
    "P8D" = "Endoscopy: Colonoscopy",
    "P8E" = "Endoscopy: Cystoscopy",
    "P8F" = "Endoscopy: Bronchoscopy",
    "P8G" = "Endoscopy: Laparoscopic Cholecystectomy",
    "P8H" = "Endoscopy: Laryngoscopy",
    "P8I" = "Endoscopy: Other",
    "P9A" = "Dialysis Services (Medicare Fee Schedule)",
    "P9B" = "Dialysis Services (Non-Medicare Fee Schedule)",
    "T1A" = "Lab tests: Routine Venipuncture (Non-Medicare Fee Schedule)",
    "T1B" = "Lab tests: Automated General Profiles",
    "T1C" = "Lab tests: Urinalysis",
    "T1D" = "Lab tests: Blood Counts",
    "T1E" = "Lab tests: Glucose",
    "T1F" = "Lab tests: Bacterial Cultures",
    "T1G" = "Lab tests: Other (Medicare Fee Schedule)",
    "T1H" = "Lab tests: Other (Non-Medicare Fee Schedule)",
    "T2A" = "Other tests: ECGs",
    "T2B" = "Other tests: Cardiovascular Stress Tests",
    "T2C" = "Other tests: EKG Monitoring",
    "T2D" = "Other tests: Other",
    "Y1"  = "Other: Medicare Fee Schedule",
    "Y2"  = "Other: Non-Medicare Fee Schedule",
    "Z1"  = "Local Codes",
    "Z2"  = "Undefined Codes"
  )
}
