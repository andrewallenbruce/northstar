
#' ref <- function(type = c("pfs", "rvu", "lvl2")) {
#'
#'   type <- match.arg(type)
#'
#'   if (type == "pfs") {
#'     return(
#'       list(
#'         stat = status_codes(),
#'         mod  = modifiers(),
#'         mult = multiple_procedure(),
#'         opps = opps_ind()
#'       ))
#'   }
#'
#'   if (type == "rvu") {
#'     return(
#'       list(
#'         glob = global_days(),
#'         stat = status_codes(),
#'         mod  = modifiers(),
#'         pctc = pctc_ind(),
#'         img  = diagnostic_imaging(),
#'         phys = physician_supervision(),
#'         team = team_surgery(),
#'         co   = co_surgeons(),
#'         asst = assistant_surgery(),
#'         bil  = bilateral_surgery(),
#'         mult = multiple_procedure(),
#'         rare = rarely()
#'       ))
#'   }
#'
#'   if (type == "lvl2") {
#'     return(
#'       list(
#'         asc     = asc_group(),
#'         cov     = coverage(),
#'         action  = action_cd(),
#'         price   = price_indicator(),
#'         mult    = multiple_price_indicator(),
#'         betos   = betos(),
#'         lab     = labcert(),
#'         tos     = type_of_service()
#'       ))
#'   }
#' }

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
    "hcpcs",     "HCPCS Code",                       "HCPCS Procedure Code",
    "mod",       "Modifier",                         "For diagnostic tests, a blank in this field denotes the global service, Mods 26 & TC identify components. Mod 53 indicates separate RVUs and fee schedule amount have been established for procedures terminated before completion.",
    "status",    "Status Code",                      "Indicates if code is in fee schedule, if service is covered, whether separately payable. Only codes **A**, **R** and **T** used for Medicare payment.",
    "mult_surg", "Multiple Surgery Indicator",       "Indicates applicable payment adjustment rule: Mod 51",
    "flat_vis",  "Flat Rate Visit Fee",              "Contains Flat Visit Fee for Primary Care First Model",
    "nther",     "Non-Facility Therapy Reduction",   "Fee reflects 50% PE payment for Non-facility services",
    "fther",     "Facility Therapy Reduction",       "Fee reflects 50% PE payment for Facility services",
    "fee_nf",    "Non-Facility Fee Schedule Amount", "Non-facility Pricing amount",
    "fee_f",     "Facility Fee Schedule Amount",     "Facility Pricing amount",
    "opps",      "OPPS Indicator",                   "OPPS Payment Cap Determination: `1` = Applies, `9` = Does not Apply",
    "opps_nf",   "OPPS Non-Facility",                "OPPS Capped Non-facility Pricing amount",
    "opps_f",    "OPPS Facility",                    "OPPS Capped Facility Pricing amount"
  )
}

#' @autoglobal
#' @noRd
rvu_lb <- function() {
  dplyr::tribble(
    ~var,        ~  label,                                         ~description,
    #----          #-----                                          #-----------
    "hcpcs",       "HCPCS Code",                                   "HCPCS Procedure Code",
    "description", "Description",                                  "HCPCS Procedure Description",
    "mod",         "Modifier",                                     "For diagnostic tests, a blank in this field denotes the global service and Modifiers 26 & TC identify the components. Modifier 53 indicates that separate RVUs and a fee schedule amount have been established for procedures which the physician terminated before completion.",
    "status",      "Status Code",                                  "Indicates whether the code is in the fee schedule and whether it is separately payable if the service is covered. Only RVUs associated with status codes of A, R, or T, are used for Medicare payment.",
    "wrvu",        "Work RVU",                                     "RVU for Physician Work",
    "nprvu",       "Non-Facility Practice Expense RVU",            "RVU for Non-facility Practice Expense",
    "fprvu",       "Facility Practice Expense RVU",                "RVU for Facility Practice Expense",
    "mrvu",        "Malpractice RVU",                              "RVU for Malpractice Expense",
    "cf",          "Conversion Factor",                            "Multiplier that transforms RVUs into payment amounts",
    "nprvu_opps",  "Non-Facility PE Used for OPPS Payment Amount", "Non-Facility Practice Expense RVUs used for OPPS payment",
    "fprvu_opps",  "Facility PE Used for OPPS Payment Amount",     "Facility Practice Expense RVUs used for OPPS payment",
    "global",      "Global Days",                                  "Identifies Number of Global Days",
    "op_ind",      "Operative Percentage Indicator",               "1 = Has percentages, 0 = Does not have percentages",
    "op_pre",      "Preoperative Percentage",                      "Preoperative % of Global package",
    "op_intra",    "Intraoperative Percentage",                    "Intraoperative % of Global package, including Postoperative work in the hospital",
    "op_post",     "Postoperative Percentage",                     "Postoperative % of Global package, provided in-office, post-hospital discharge",
    "pctc",        "PCTC Indicator",                               "PCTC Payment Adjustment",
    "mult_proc",   "Multiple Procedure Indicator",                 "Multiple Procedures (Mod 51) Payment Adjustment",
    "surg_bilat",  "Bilateral Surgery Indicator",                  "Bilateral Procedures (Mod 50) Payment Adjustment",
    "surg_asst",   "Assistant Surgery Indicator",                  "Assistant at Surgery (Mods 80, 81, 82, or AS) Payment Adjustment",
    "surg_co",     "Co-Surgery Indicator",                         "Co-surgeons (Mod 62) Payment Adjustment",
    "surg_team",   "Team Surgery Indicator",                       "team surgeons (Mod 66) Payment Adjustment",
    "endo",        "Endoscopic Base Code",                         "Endoscopic base code for HCPCS with Multiple Surgery indicator of **3**",
    "supvis",      "Physician Supervision Indicator",              "Physician supervision level required for service",
    "dximg",       "Diagnostic Imaging Family Indicator",          "Diagnostic Service Family for HCPCS with Multiple Procedure indicator of **4**",
    "unused",      "Not Used for Medicare Payment",                "Whether code is used for Medicare payment or Not",
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
asc_group <- function() {
  c("YY" = "Procedure approved to be performed in an ASC",
    "NA" = "Not Approved For ASC")
}

#' @autoglobal
#' @noRd
coverage <- function() {
  c("C" = "Carrier Judgment",
    "D" = "Special Coverage Instructions Apply",
    "I" = "Not Payable by Medicare",
    "M" = "Non-Covered by Medicare",
    "S" = "Non-Covered by Medicare Statute")
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
price_indicator <- function() {
  c(
    "00" = "Service not separately priced by part B (e.g., services not covered, bundled, used by part a only, etc.)",
    "11" = "Price established using National RVUs",
    "12" = "Price established using national anesthesia base units",
    "13" = "Price established by carriers (e.g., not otherwise classified, individual determination, carrier discretion)",
    "21" = "Price subject to national limitation amount",
    "22" = "Price established by carriers (e.g., gap-fills, carrier established panels)",
    "31" = "Frequently serviced DME (price subject to floors and ceilings)",
    "32" = "Inexpensive & routinely purchased DME (price subject to floors and ceilings)",
    "33" = "Oxygen and oxygen equipment (price subject to floors and ceilings)",
    "34" = "DME supplies (price subject to floors and ceilings)",
    "35" = "Surgical dressings (price subject to floors and ceilings)",
    "36" = "Capped rental DME (price subject to floors and ceilings)",
    "37" = "Ostomy, tracheostomy and urological supplies (price subject to floors and ceilings)",
    "38" = "Orthotics, prosthetics, prosthetic devices & vision services (price subject to floors and ceilings)",
    "39" = "Parenteral and Enteral Nutrition",
    "40" = "Lymphedema Compression Treatment Items",
    "45" = "Customized DME items",
    "46" = "Carrier priced (e.g., not otherwise classified, individual determination, carrier discretion, gap-filled amounts)",
    "51" = "Drugs",
    "52" = "Reasonable charge",
    "53" = "Statute",
    "54" = "Vaccinations",
    "55" = "Splints and Casts",
    "56" = "IOL's inserted in a physician's office",
    "57" = "Other carrier priced",
    "99" = "Value not established")
}


#' @autoglobal
#' @noRd
multiple_price_indicator <- function() {

  c(
    "9" = "Not applicable as HCPCS not priced separately by part B (pricing indicator is 00) or value is not established (pricing indicator is 99)",
    "A" = "Not applicable as HCPCS priced under one methodology",
    "B" = "Professional component of HCPCS priced using RVUs, while technical component and global service priced by Medicare part B carriers",
    "C" = "Physician interpretation of clinical lab service is priced under physician fee schedule using RVUs, while pricing of lab service is paid under clinical lab fee schedule",
    "D" = "Service performed by physician is priced under physician fee schedule using RVUs, while service performed by clinical psychologist is priced under clinical psychologist fee schedule (not applicable as of January 1, 1998)",
    "E" = "Service performed by physician is priced under physician fee schedule using RVUs, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)",
    "F" = "Service performed by physician is priced under physician fee schedule by carriers, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)",
    "G" = "Clinical lab service priced under reasonable charge when service is submitted on claim with blood products, while service is priced under clinical lab fee schedule when there are no blood products on claim.")
}

#' @autoglobal
#' @noRd
labcert <- function() {
  c(
    "010" = "Histocompatibility testing",
    "100" = "Microbiology",
    "110" = "Bacteriology",
    "115" = "Mycobacteriology",
    "120" = "Mycology",
    "130" = "Parasitology",
    "140" = "Virology",
    "150" = "Other microbiology",
    "200" = "Diagnostic immunology",
    "210" = "Syphilis serology",
    "220" = "General immunology",
    "300" = "Chemistry",
    "310" = "Routine chemistry",
    "320" = "Urinalysis",
    "330" = "Endocrinology",
    "340" = "Toxicology",
    "350" = "Other chemistry",
    "400" = "Hematology",
    "500" = "Immunohematology",
    "510" = "Abo group & RH type",
    "520" = "Antibody detection (transfusion)",
    "530" = "Antibody detection (nontransfusion)",
    "540" = "Antibody identification",
    "550" = "Compatibility testing",
    "560" = "Other immunohematology",
    "600" = "Pathology",
    "610" = "Histopathology",
    "620" = "Oral pathology",
    "630" = "Cytology",
    "800" = "Radiobioassay",
    "900" = "Clinical cytogenetics")
}

#' @autoglobal
#' @noRd
betos <- function() {
  c(
    "D1A" = "Medical/surgical supplies",
    "D1B" = "Hospital beds",
    "D1C" = "Oxygen and supplies",
    "D1D" = "Wheelchairs",
    "D1E" = "Other DME",
    "D1F" = "Prosthetic/Orthotic devices",
    "D1G" = "Drugs Administered through DME",
    "I1A" = "Standard imaging - chest",
    "I1B" = "Standard imaging - musculoskeletal",
    "I1C" = "Standard imaging - breast",
    "I1D" = "Standard imaging - contrast gastrointestinal",
    "I1E" = "Standard imaging - nuclear medicine",
    "I1F" = "Standard imaging - other",
    "I2A" = "Advanced imaging - CAT/CT/CTA: brain/head/neck",
    "I2B" = "Advanced imaging - CAT/CT/CTA: other",
    "I2C" = "Advanced imaging - MRI/MRA: brain/head/neck",
    "I2D" = "Advanced imaging - MRI/MRA: other",
    "I3A" = "Echography/ultrasonography - eye",
    "I3B" = "Echography/ultrasonography - abdomen/pelvis",
    "I3C" = "Echography/ultrasonography - heart",
    "I3D" = "Echography/ultrasonography - carotid arteries",
    "I3E" = "Echography/ultrasonography - prostate, transrectal",
    "I3F" = "Echography/ultrasonography - other",
    "I4A" = "Imaging/procedure - heart including cardiac catheterization",
    "I4B" = "Imaging/procedure - other",
    "M1A" = "Office visits - new",
    "M1B" = "Office visits - established",
    "M2A" = "Hospital visit - initial",
    "M2B" = "Hospital visit - subsequent",
    "M2C" = "Hospital visit - critical care",
    "M3"  = "Emergency room visit",
    "M4A" = "Home visit",
    "M4B" = "Nursing home visit",
    "M5A" = "Specialist - pathology",
    "M5B" = "Specialist - psychiatry",
    "M5C" = "Specialist - opthamology",
    "M5D" = "Specialist - other",
    "M6"  = "Consultations",
    "O1A" = "Ambulance",
    "O1B" = "Chiropractic",
    "O1C" = "Enteral and parenteral",
    "O1D" = "Chemotherapy",
    "O1E" = "Other drugs",
    "O1F" = "Hearing and speech services",
    "O1G" = "Immunizations/Vaccinations",
    "01L" = "Lymphedema Compression Treatment Items (eff 1/1/2024)",
    "P0"  = "Anesthesia",
    "P1A" = "Major procedure - breast",
    "P1B" = "Major procedure - colectomy",
    "P1C" = "Major procedure - cholecystectomy",
    "P1D" = "Major procedure - turp",
    "P1E" = "Major procedure - hysterectomy",
    "P1F" = "Major procedure - explor/decompr/excisdisc",
    "P1G" = "Major procedure - other",
    "P2A" = "Major procedure, cardiovascular-CABG",
    "P2B" = "Major procedure, cardiovascular-Aneurysm repair",
    "P2C" = "Major Procedure, cardiovascular-Thromboendarterectomy",
    "P2D" = "Major procedure, cardiovascular-Coronary angioplasty (PTCA)",
    "P2E" = "Major procedure, cardiovascular-Pacemaker insertion",
    "P2F" = "Major procedure, cardiovascular-Other",
    "P3A" = "Major procedure, orthopedic - Hip fracture repair",
    "P3B" = "Major procedure, orthopedic - Hip replacement",
    "P3C" = "Major procedure, orthopedic - Knee replacement",
    "P3D" = "Major procedure, orthopedic - other",
    "P4A" = "Eye procedure - corneal transplant",
    "P4B" = "Eye procedure - cataract removal/lens insertion",
    "P4C" = "Eye procedure - retinal detachment",
    "P4D" = "Eye procedure - treatment of retinal lesions",
    "P4E" = "Eye procedure - other",
    "P5A" = "Ambulatory procedures - skin",
    "P5B" = "Ambulatory procedures - musculoskeletal",
    "P5C" = "Ambulatory procedures - inguinal hernia repair",
    "P5D" = "Ambulatory procedures - lithotripsy",
    "P5E" = "Ambulatory procedures - other",
    "P6A" = "Minor procedures - skin",
    "P6B" = "Minor procedures - musculoskeletal",
    "P6C" = "Minor procedures - other (Medicare fee schedule)",
    "P6D" = "Minor procedures - other (Non-Medicare fee schedule)",
    "P7A" = "Oncology - radiation therapy",
    "P7B" = "Oncology - other",
    "P8A" = "Endoscopy - arthroscopy",
    "P8B" = "Endoscopy - upper gastrointestinal",
    "P8C" = "Endoscopy - sigmoidoscopy",
    "P8D" = "Endoscopy - colonoscopy",
    "P8E" = "Endoscopy - cystoscopy",
    "P8F" = "Endoscopy - bronchoscopy",
    "P8G" = "Endoscopy - laparoscopic cholecystectomy",
    "P8H" = "Endoscopy - laryngoscopy",
    "P8I" = "Endoscopy - other",
    "P9A" = "Dialysis services (Medicare fee schedule)",
    "P9B" = "Dialysis services (Non-Medicare fee schedule)",
    "T1A" = "Lab tests - routine venipuncture (Non-Medicare fee schedule)",
    "T1B" = "Lab tests - automated general profiles",
    "T1C" = "Lab tests - urinalysis",
    "T1D" = "Lab tests - blood counts",
    "T1E" = "Lab tests - glucose",
    "T1F" = "Lab tests - bacterial cultures",
    "T1G" = "Lab tests - other (Medicare fee schedule)",
    "T1H" = "Lab tests - other (Non-Medicare fee schedule)",
    "T2A" = "Other tests - electrocardiograms",
    "T2B" = "Other tests - cardiovascular stress tests",
    "T2C" = "Other tests - EKG monitoring",
    "T2D" = "Other tests - other",
    "Y1"  = "Other - Medicare fee schedule",
    "Y2"  = "Other - Non-Medicare fee schedule",
    "Z1"  = "Local codes",
    "Z2"  = "Undefined codes"
    )

}

#' @autoglobal
#' @noRd
type_of_service <- function() {
  c(
    "1" = "Medical care",
    "2" = "Surgery",
    "3" = "Consultation",
    "4" = "Diagnostic radiology",
    "5" = "Diagnostic laboratory",
    "6" = "Therapeutic radiology",
    "7" = "Anesthesia",
    "8" = "Assistant at surgery",
    "9" = "Other medical items or services",
    "0" = "Whole blood only eff 01/96, whole blood or packed red cells before 01/96",
    "A" = "Used durable medical equipment (DME)",
    "B" = "High risk screening mammography (obsolete 1/1/98)",
    "C" = "Low risk screening mammography (obsolete 1/1/98)",
    "D" = "Ambulance (eff 04/95)",
    "E" = "Enteral/parenteral nutrients/supplies (eff 04/95)",
    "F" = "Ambulatory surgical center (facility usage for surgical services)",
    "G" = "Immunosuppressive drugs",
    "H" = "Hospice services (discontinued 01/95)",
    "I" = "Purchase of DME (installment basis) (discontinued 04/95)",
    "J" = "Diabetic shoes (eff 04/95)",
    "K" = "Hearing items and services (eff 04/95)",
    "L" = "ESRD supplies (eff 04/95) (renal supplier in the home before 04/95)",
    "M" = "Monthly capitation payment for dialysis",
    "N" = "Kidney donor",
    "P" = "Lump sum purchase of DME, prosthetics, orthotics",
    "Q" = "Vision items or services",
    "R" = "Rental of DME",
    "S" = "Surgical dressings or other medical supplies (eff 04/95)",
    "T" = "Psychological therapy (term. 12/31/97) outpatient mental health limitation (eff. 1/1/98)",
    "U" = "Occupational therapy",
    "V" = "Pneumococcal/flu vaccine (eff 01/96), Pneumococcal/flu/hepatitis B vaccine (eff 04/95-12/95), Pneumococcal only before 04/95",
    "W" = "Physical therapy",
    "Y" = "Second opinion on elective surgery (obsoleted 1/97)",
    "Z" = "Third opinion on elective surgery (obsoleted 1/97)"
  )
}
