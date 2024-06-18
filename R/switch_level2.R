#' HCPCS Level II Physician Supervision Descriptions
#'
#' This field is for use in post payment review.
#'
#' @param x vector of Physician Supervision indicators
#'
#' @returns vector of Physician Supervision descriptions
#'
#' @examples
#' dplyr::tibble(
#'   code = c(
#'     fuimus::pad_number(c(1:6, 9, 21:22, 66, 77)), "6A", "7A"),
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

#' HCPCS Level II ASC Group Descriptions
#'
#' @param x vector of ASC Group indicators
#'
#' @returns vector of ASC Group descriptions
#'
#' @examples
#' dplyr::tibble(code = c("YY", "99"), desc = switch_asc_group(code))
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

#' HCPCS Level II Coverage Descriptions
#'
#' @param x vector of Coverage indicators
#'
#' @returns vector of Coverage descriptions
#'
#' @examples
#' dplyr::tibble(code = c("C", "D", "I", "M", "S"), desc = switch_coverage(code))
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

#' HCPCS Level II Pricing Descriptions
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

#' HCPCS Level II Multiple Pricing Descriptions
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

#' HCPCS Level II Lab Certification Descriptions
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

#' HCPCS Level II Type of Service Descriptions
#'
#' @param x vector of Type of Service codes
#'
#' @returns vector of Type of Service descriptions
#'
#' @examples
#' dplyr::tibble(code = c(0:9, LETTERS), desc = switch_type_of_service(code))
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

#' HCPCS Level II Action Descriptions
#'
#' @param x vector of Action codes
#'
#' @returns vector of Action descriptions
#'
#' @examples
#' dplyr::tibble(code = c(LETTERS), desc = switch_action_code(code))
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

#' HCPCS Level II BETOS Descriptions
#'
#' @param x vector of BETOS codes
#'
#' @returns vector of BETOS descriptions
#'
#' @examples
#' dplyr::tibble(code = c("D1A", "I2A", "M3", "P0"), desc = switch_betos(code))
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
