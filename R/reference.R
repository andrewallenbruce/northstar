#' @autoglobal
#' @noRd
col_lb <- function(output = c("md", "df"), type = c("pfs", "rvu")) {

  output <- match.arg(output)
  type <- match.arg(type)

  if (type == "pfs") {res <- pfs_lb()}
  if (type == "rvu") {res <- rvu_lb()}

  if (output == "df") {return(res)}

  if (output == "md") {
    return(
      res |>
        dplyr::mutate(var         = gluedown::md_code(var),
                      label       = gluedown::md_bold(label),
                      description = gluedown::md_hardline(description))
    )
  }
}

#' @autoglobal
#' @noRd
pfs_lb <- function() {
  dplyr::tribble(
    ~var,        ~label,                              ~description,
    #----        #-----                              #-----------
    "mac",       "Carrier Number",                   "Medicare Administrative Contractor ID",
    "locality",  "Locality",                         "Pricing Locality ID",
    "hcpcs",     "HCPCS Code",                       "HCPCS Code",
    "mod",       "Modifier",                         "Diagnostic Tests, `NA` denotes Global Service, Mods `26` & `TC` identify Components. Mod `53` indicates Separate RVUs & PFS Amount for Procedures Terminated Before Completion.",
    "status",    "Status Code",                      "Indicates if in Fee Schedule, if Covered, if Separately Payable. Only `A`, `R` and `T` used for Medicare payment.",
    "mult_surg", "Multiple Surgery Indicator",       "Indicates Applicable Payment Adjustment Rule: Mod `51`",
    "flat_vis",  "Flat Rate Visit Fee",              "Contains Flat Visit Fee for Primary Care First Model",
    "nther",     "Non-Facility Therapy Reduction",   "Fee reflects 50% PE payment for Non-facility services",
    "fther",     "Facility Therapy Reduction",       "Fee reflects 50% PE payment for Facility services",
    "fee_nf",    "Non-Facility Fee Schedule Amount", "Non-Facility Pricing Amount",
    "fee_f",     "Facility Fee Schedule Amount",     "Facility Pricing Amount",
    "opps",      "OPPS Indicator",                   "OPPS Payment Cap Determination: `1` = Applies, `9` = Does Not Apply",
    "opps_nf",   "OPPS Non-Facility",                "OPPS Capped Non-Facility Pricing Amount",
    "opps_f",    "OPPS Facility",                    "OPPS Capped Facility Pricing Amount"
  )
}

#' @autoglobal
#' @noRd
rvu_lb <- function() {
  dplyr::tribble(
    ~var,        ~  label,                                         ~description,
    #----          #-----                                          #-----------
    "hcpcs",       "HCPCS Code",                                   "HCPCS Code",
    "description", "Description",                                  "HCPCS Procedure Description",
    "mod",         "Modifier",                                     "Diagnostic Tests, `NA` denotes Global Service, Mods `26` & `TC` identify Components. Mod `53` indicates Separate RVUs & PFS Amount for Procedures Terminated Before Completion.",
    "status",      "Status Code",                                  "Indicates if in Fee Schedule, if Covered, if Separately Payable. Only `A`, `R` and `T` used for Medicare payment.",
    "wrvu",        "Work RVU",                                     "RVUs for Physician Work",
    "nprvu",       "Non-Facility Practice Expense RVU",            "RVUs for Non-Facility Practice Expense",
    "fprvu",       "Facility Practice Expense RVU",                "RVUs for Facility Practice Expense",
    "mrvu",        "Malpractice RVU",                              "RVUs for Malpractice Expense",
    "cf",          "Conversion Factor",                            "Multiplier that Transforms RVUs into Payment Amounts",
    "nprvu_opps",  "Non-Facility PE Used for OPPS Payment Amount", "Non-Facility Practice Expense RVUs for OPPS Payment",
    "fprvu_opps",  "Facility PE Used for OPPS Payment Amount",     "Facility Practice Expense RVUs for OPPS Payment",
    "global",      "Global Days",                                  "Number of Global Days",
    "op_ind",      "Operative Percentage Indicator",               "1 = Has percentages, 0 = Does not have percentages",
    "op_pre",      "Preoperative Percentage",                      "Preoperative % of Global Package",
    "op_intra",    "Intraoperative Percentage",                    "Intraoperative % of Global Package, including Postoperative Work in Hospital",
    "op_post",     "Postoperative Percentage",                     "Postoperative % of Global Package, Provided in Office, Post-Discharge",
    "pctc",        "PCTC Indicator",                               "PCTC Payment Adjustment",
    "mult_proc",   "Multiple Procedure Indicator",                 "Multiple Procedures (Mod 51) Payment Adjustment",
    "surg_bilat",  "Bilateral Surgery Indicator",                  "Bilateral Procedure (Mod 50) Payment Adjustment",
    "surg_asst",   "Assistant Surgery Indicator",                  "Assistant at Surgery (Mods 80, 81, 82, or AS) Payment Adjustment",
    "surg_co",     "Co-Surgery Indicator",                         "Co-surgeons (Mod 62) Payment Adjustment",
    "surg_team",   "Team Surgery Indicator",                       "Team Surgeons (Mod 66) Payment Adjustment",
    "endo",        "Endoscopic Base Code",                         "Endoscopic Base Code for HCPCS with Multiple Surgery indicator **3**",
    "supvis",      "Physician Supervision Indicator",              "Physician Supervision Level Required for Service",
    "dximg",       "Diagnostic Imaging Family Indicator",          "Diagnostic Service Family for HCPCS with Multiple Procedure indicator **4**",
    "unused",      "Not Used for Medicare Payment",                "Whether Code used for Medicare Payment",
    "rare",        "Rarely/Never Performed",                       "Procedure rarely/never performed in: `00` (Neither), `01` (Facility), `10` (Non-Facility), `11` (Both)"
  )
}

#' @autoglobal
#' @noRd
rarely <- function() {
  c("00" = "Concept does not apply",
    "01" = "Rarely/never performed in Facility setting",
    "10" = "Rarely/never performed in Non-facility setting",
    "11" = "Rarely/never performed in Facility or Non-facility setting")
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
