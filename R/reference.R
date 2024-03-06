#' @noRd
global_days <- function() {
  c("000" = "Endoscopic or minor procedure with related preoperative and postoperative relative values on the day of the procedure only included in the fee schedule payment amount. Evaluation and Management services on the day of the procedure generally not payable.",
    "010" = "Minor procedure with preoperative relative values on the day of the procedure and postoperative relative values during a 10-day postoperative period included in the fee schedule amount. Evaluation and Management services on the day of the procedure and during the 10-day postoperative period generally not payable.",
    "090" = "Major surgery with a 1-day preoperative period and 90-day postoperative period included in the fee schedule amount.",
    "MMM" = "Maternity codes; usual global period does not apply.",
    "XXX" = "Global concept does not apply.",
    "YYY" = "Carrier determines whether the global concept applies and establishes postoperative period, if appropriate, at time of pricing.",
    "ZZZ" = "Code is related to another service and is always included in the global period of the other service.")
}

#' @noRd
pctc_ind <- function() {
  list(
    "0" = c(label = "Physician Service Codes", description = "Physician services. The concept of PC/TC does not apply since physician services cannot be split into professional and technical components. Modifiers 26 and TC cannot be used with these codes. The RVUS include values for physician work, practice expense and malpractice expense. There are some codes with no work RVUs."),
    "1" = c(label = "Diagnostic Tests for Radiology Services", description = "Diagnostic tests. These codes have both a professional and technical component. Modifiers 26 and TC can be used with these codes. The total RVUs for codes reported with a 26 modifier include values for physician work, practice expense, and malpractice expense. The total RVUs for codes reported with a TC modifier include values for practice expense and malpractice expense only. The total RVUs for codes reported without a modifier include values for physician work, practice expense, and malpractice expense."),
    "2" = c(label = "Professional Component Only Codes", description = "Standalone codes that describe the physician work portion of selected diagnostic tests for which there is an associated code that describes the technical component of the diagnostic test only and another associated code that describes the global test. The total RVUs for professional component only codes include values for physician work, practice expense, and malpractice expense."),
    "3" = c(label = "Technical Component Only Codes", description = "Standalone codes that describe the technical component (i.e., staff and equipment costs) of selected diagnostic tests for which there is an associated code that describes the professional component of the diagnostic test only. Also identifies codes that are covered only as diagnostic tests and therefore do not have a related professional code. Modifiers 26 and TC cannot be used with these codes. The total RVUs for technical component only codes include values for practice expense and malpractice expense only."),
    "4" = c(label = "Global Test Only Codes", description = "Standalone codes that describe selected diagnostic tests for which there are associated codes that describe 1. the professional component of the test only, and 2. the technical component of the test only. Modifiers 26 and TC cannot be used with these codes. The total RVUs for global procedure only codes include values for physician work, practice expense, and malpractice expense. The total RVUs for global procedure only codes equals the sum of the total RVUs for the professional and technical components only codes combined."),
    "5" = c(label = "Incident To Codes", description = "Services covered incident to a physician's service when they are provided by auxiliary personnel employed by the physician and working under his or her direct personal supervision. Payment may not be made by A/B MACs (B) for these services when they are provided to hospital inpatients or patients in a hospital outpatient department. Modifiers 26 and TC cannot be used with these codes."),
    "6" = c(label = "Laboratory Physician Interpretation Codes", description = "Clinical laboratory codes for which separate payment for interpretations by laboratory physicians may be made. Actual performance of the tests is paid for under the lab fee schedule. Modifier TC cannot be used with these codes. The total RVUs for laboratory physician interpretation codes include values for physician work, practice expense, and malpractice expense."),
    "7" = c(label = "Physical therapy service, for which payment may not be made", description = "Payment may not be made if the service is provided to either a patient in a hospital outpatient department or to an inpatient of the hospital by an independently practicing physical or occupational therapist."),
    "8" = c(label = "Physician interpretation codes", description = "Identifies the professional component of clinical laboratory codes for which separate payment may be made only if the physician interprets an abnormal smear for hospital inpatient. This applies to CPT codes 85060. No TC billing is recognized because payment for the underlying clinical laboratory test is made to the hospital, generally through the PPS rate. No payment is recognized for CPT codes 85060 furnished to hospital outpatients or non-hospital patients. The physician interpretation is paid through the clinical laboratory fee schedule payment for the clinical laboratory test."),
    "9" = c(label = "Not Applicable", description = "Concept of a professional/technical component does not apply"))
}

#' @autoglobal
#' @noRd
status_codes <- function() {

  list(
    "A" = c(label = "Active Code", description = "Separately paid under the Physician Fee Schedule if covered. There will be RVUs and payment amounts. Does not mean that Medicare has made a National Coverage Determination regarding the service. Carriers remain responsible for coverage decisions in the absence of a national Medicare policy."),
    "B" = c(label = "Payment Bundled", description = "Payment for covered services are always bundled into payment for other services not specified. No RVUs or payment amounts and no separate payment is ever made. When these services are covered, payment for them is subsumed by the payment for the services to which they are incident. Example: telephone call from a hospital nurse regarding care of a patient."),
    "C" = c(label = "Carrier Priced", description = "Carriers will establish RVUs and payment amounts for these services, generally on an individual case basis following review of documentation such as an operative report."),
    "D" = c(label = "Deleted Codes", description = "Deleted effective with the beginning of the applicable year."),
    "E" = c(label = "Regulatory Exclusion", description = "Item or service that CMS chose to exclude from the fee schedule payment by regulation. No RVUs or payment amounts are shown and no payment may be made under the fee schedule. Payment for them, when covered, continues under reasonable charge procedures."),
    "F" = c(label = "Deleted/Discontinued Codes", description = "Code not subject to a 90 day grace period"),
    "G" = c(label = "Not Valid for Medicare Purposes", description = "Medicare uses another code for reporting of, and payment for, these services. Code subject to a 90 day grace period."),
    "H" = c(label = "Deleted Modifier", description = "Had an associated TC and/or 26 modifier in the previous year. For the current year, the TC or 26 component shown for the code has been deleted, and the deleted component is shown with a status code of H."),
    "I" = c(label = "Not Valid for Medicare Purposes", description = "Medicare uses another code for reporting of, and payment for, these services. Code is NOT subject to a 90-day grace period."),
    "J" = c(label = "Anesthesia Service", description = "No RVUs or payment amounts for anesthesia codes on the database, only used to facilitate the identification of anesthesia services."),
    "M" = c(label = "Measurement Code", description = "Used for reporting purposes only."),
    "N" = c(label = "Restricted Coverage", description = "Not covered by Medicare."),
    "P" = c(label = "Non-Covered Service", description = "No RVUs and no payment amounts for these services. No separate payment is made for them under the fee schedule. If the item or service is covered as incident to a physician service and is provided on the same day as a physician service, payment for it is bundled into the payment for the physician service to which it is incident (an example is an elastic bandage furnished by a physician incident to a physician service). If the item or service is covered as other than incident to a physician service, it is excluded from the fee schedule (for example, colostomy supplies) and is paid under the other payment provision of the Act."),
    "R" = c(label = "Bundled/Excluded Code", description = "Special coverage instructions apply. If covered, the service is contractor priced. NOTE: The majority of codes to which this indicator will be assigned are the alpha-numeric dental codes, which begin with D. We are assigning the indicator to a limited number of CPT codes which represent services that are covered only in unusual circumstances."),
    "T" = c(label = "Injections", description = "There are RVUs and payment amounts for these services, but they are only paid if there are no other services payable under the physician fee schedule billed on the same date by the same provider. If any other services payable under the physician fee schedule are billed on the same date by the same provider, these services are bundled into the physician services for which payment is made. NOTE: This is a change from the previous definition, which states that injection services are bundled into any other services billed on the same date."),
    "X" = c(label = "Statutory Exclusion", description = "Item or service that is not in the statutory definition of 'physician services' for fee schedule payment purposes. No RVUs or payment amounts are shown for these codes and no payment may be made under the physician fee schedule. Ex: Ambulance Services and Clinical Diagnostic Laboratory Services."))
}

#' @autoglobal
#' @noRd
diagnostic_imaging <- function() {
  # Identifies the applicable diagnostic service family for
  # HCPCS codes with a multiple procedure indicator of ‘4’.
  c("01" = "Ultrasound (Chest / Abdomen / Pelvis-Non-Obstetrical)",
    "02" = "CT and CTA (Chest / Thorax / Abd / Pelvis)",
    "03" = "CT and CTA (Head / Brain / Orbit / Maxillofacial / Neck)",
    "04" = "MRI and MRA (Chest / Abd / Pelvis)",
    "05" = "MRI and MRA (Head / Brain / Neck)",
    "06" = "MRI and MRA (Spine)",
    "07" = "CT (Spine)",
    "08" = "MRI and MRA (Lower Extremities)",
    "09" = "CT and CTA (Lower Extremities)",
    "10" = "MRI and MRA (Upper Extremities and Joints)",
    "11" = "CT and CTA (Upper Extremities)",
    "88" = "Subject to the reduction of the TC diagnostic imaging (effective for services January 1, 2011 and after). Subject to the reduction of the PC diagnostic imaging (effective for services January 1, 2012, and after)",
    "99" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
physician_supervision <- function() {
  # This field is for use in post payment review.
  c("01" = "Procedure must be performed under the general supervision of a physician.",
    "02" = "Procedure must be performed under the direct supervision of a physician.",
    "03" = "Procedure must be performed under the personal supervision of physician.",
    "04" = "Physician supervision policy does not apply when procedure is furnished by a qualified, independent psychologist or a clinical psychologist; otherwise must be performed under the general supervision of a physician.",
    "05" = "Physician supervision policy does not apply when procedure is furnished by a qualified audiologist; otherwise must be performed under the general supervision of a physician.",
    "06" = "Procedure must be performed by a physician or a physical therapist (PT) who is certified by the American Board of Physical Therapy Specialties (ABPTS) as a qualified electrophysiological clinical specialist and is permitted to provide the procedure under State law.",
    "21" = "Procedure may be performed by a technician with certification under general supervision of a physician; otherwise must be performed under direct supervision of a physician.",
    "22" = "May be performed by a technician with on-line real-time contact with physician.",
    "66" = "May be performed by a physician or by a physical therapist with ABPTS certification and certification in this specific procedure.",
    "6A" = "Supervision standards for level 66 apply; in addition, the PT with ABPTS certification may supervise another PT, but only the PT with ABPTS certification may bill.",
    "77" = "Procedure must be performed by a PT with ABPTS certification or by a PT without certification under direct supervision of a physician, or by a technician with certification under general supervision of a physician.",
    "7A" = "Supervision standards for level 77 apply; in addition, the PT with ABPTS certification may supervise another PT, but only the PT with ABPTS certification may bill.",
    "09" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
team_surgery <- function() {
  # Modifier 66: Indicates services for which team surgeons may be paid.
  c("0" = "Team surgeons not permitted for this procedure.",
    "1" = "Team surgeons could be paid, though supporting documentation required to establish medical necessity of a team; pay by report.",
    "2" = "Team surgeons permitted; pay by report.",
    "9" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
co_surgeons <- function() {
  # Modifier 62: Indicates services for which two surgeons, each in a different specialty, may be paid.
  c("0" = "Co-surgeons not permitted for this procedure.",
    "1" = "Co-surgeons could be paid, though supporting documentation required to establish medical necessity of two surgeons; pay by report.",
    "2" = "Co-surgeons permitted; pay by report.",
    "9" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
assistant_surgery <- function() {
  # Indicates services where an assistant at surgery is never paid for per Medicare Claims Manual.
  c("0" = "Payment restriction for assistants at surgery applies to this procedure unless supporting documentation is submitted to establish medical necessity.",
    "1" = "Statutory payment restriction for assistants at surgery applies to this procedure. Assistant at surgery may not be paid.",
    "2" = "Payment restriction for assistants at surgery does not apply to this procedure. Assistant at surgery may be paid.",
    "9" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
bilateral_surgery <- function() {
  # Modifier 50: Indicates services subject to payment adjustment.
  c("0" = "150% payment adjustment for bilateral procedures does not apply. If procedure is reported with modifier -50 or with modifiers RT and LT, base the payment for the two sides on the lower of: (a) the total actual charge for both sides and (b) 100% of the fee schedule amount for a single code. The bilateral adjustment is inappropriate for codes in this category (a) because of physiology or anatomy, or (b) because the code description specifically states that it is a unilateral procedure and there is an existing code for the bilateral procedure.",
    "1" = "150% payment adjustment for bilateral procedures applies. If the code is billed with the bilateral modifier or is reported twice on the same day by any other means (e.g., with RT and LT modifiers, or with a 2 in the units field), base the payment for these codes when reported as bilateral procedures on the lower of: (a) the total actual charge for both sides or (b) 150% of the fee schedule amount for a single code. If the code is reported as a bilateral procedure and is reported with other procedure codes on the same day, apply the bilateral adjustment before applying any multiple procedure rules.",
    "2" = "150% payment adjustment does not apply. RVUs are already based on the procedure being performed as a bilateral procedure. If the procedure is reported with modifier -50 or is reported twice on the same day by any other means (e.g., with RT and LT modifiers or with a 2 in the units field), base the payment for both sides on the lower of (a) the total actual charge by the physician for both sides, or (b) 100% of the fee schedule for a single code. ",
    "3" = "The usual payment adjustment for bilateral procedures does not apply. If the procedure is reported with modifier -50 or is reported for both sides on the same day by any other means (e.g., with RT and LT modifiers or with a 2 in the units field), base the payment for each side or organ or site of a paired organ on the lower of (a) the actual charge for each side or (b) 100% of the fee schedule amount for each side. If the procedure is reported as a bilateral procedure and with other procedure codes on the same day, determine the fee schedule amount for a bilateral procedure before applying any multiple procedure rules. Services in this category are generally radiology procedures or other diagnostic tests which are not subject to the special payment rules for other bilateral surgeries.",
    "9" = "Concept does not apply")
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
multiple_procedure <- function() {
  # Modifier 51: Indicates applicable payment adjustment rule for multiple procedures
  c("0" = "No payment adjustment rules for multiple procedures apply. If procedure is reported on the same day as another procedure, base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount for the procedure.",
    "1" = "Standard payment adjustment rules in effect before January 1, 1995 for multiple procedures apply. In the 1995 file, this indicator only applies to codes with a status code of D. If procedure is reported on the same day as another procedure that has an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 25%, 25%, 25%, and by report). Base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
    "2" = "Standard payment adjustment rules for multiple procedures apply. If procedure is reported on the same day as another procedure with an indicator of 1, 2, or 3, rank the procedures by fee schedule amount and apply the appropriate reduction to this code (100%, 50%, 50%, 50%, 50% and by report). Base the payment on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage.",
    "3" = "Special rules for multiple endoscopic procedures apply if procedure is billed with another endoscopy in the same family (i.e., another endoscopy that has the same base procedure). The base procedure for each code with this indicator is identified in the Endobase field of this file. Apply the multiple endoscopy rules to a family before ranking the family with the other procedures performed on the same day (for example, if multiple endoscopies in the same family are reported on the same day as endoscopies in another family or on the same day as a non-endoscopic procedure). If an endoscopic procedure is reported with only its base procedure, do not pay separately for the base procedure. Payment for the base procedure is included in the payment for the other endoscopy.",
    "4" = "Special rules for the technical component (TC) of diagnostic imaging procedures apply if procedure is billed with another diagnostic imaging procedure in the same family (per the diagnostic imaging family indicator, below). If procedure is reported in the same session on the same day as another procedure with the same family indicator, rank the procedures by fee schedule amount for the TC. Pay 100% for the highest priced procedure, and 50% for each subsequent procedure. Base the payment for subsequent procedures on the lower of (a) the actual charge, or (b) the fee schedule amount reduced by the appropriate percentage. Subject to 50% reduction of the TC diagnostic imaging (effective for services July 1, 2010 and after). Subject to 25% reduction of the PC of diagnostic imaging (effective for services January 1, 2012 through December 31, 2016). Subject to 5% reduction of the PC of diagnostic imaging (effective for services January 1, 2017 and after).",
    "5" = "Subject to 50% of the practice expense component for certain therapy services.",
    "6" = "Subject to 25% reduction of the second highest and subsequent procedures to the TC of diagnostic cardiovascular services, effective for services January 1, 2013, and thereafter.",
    "7" = "Subject to 20% reduction of the second highest and subsequent procedures to the TC of diagnostic ophthalmology services, effective for services January 1, 2013, and thereafter.",
    "9" = "Concept does not apply")
}

#' @autoglobal
#' @noRd
reference <- function(type = c("pfs", "hcpcs")) {

  type <- match.arg(type)

  # Endoscopic Base Code: identifies an endoscopic base code
  # for each code with a multiple surgery indicator of 3

  if (type == "pfs") {
    return(list(
      glob = global_days(),
       stat = status_codes(),
       pctc = pctc_ind(),
       img = diagnostic_imaging(),
       phys = physician_supervision(),
       team = team_surgery(),
       co = co_surgeons(),
       asst = assistant_surgery(),
       bil = bilateral_surgery(),
       mult = multiple_procedure(),
       rare = rarely()))}

  if (type == "hcpcs") {return(reference_hcpcs())}
}

#' @autoglobal
#' @noRd
reference_hcpcs <- function() {

  list(
    asc_grp = c("YY" = "Procedure approved to be performed in an ASC", "NA" = "Not Approved For ASC"),
    cov = c(
      "C" = "Carrier Judgment",
      "D" = "Special Coverage Instructions Apply",
      "I" = "Not Payable by Medicare",
      "M" = "Non-Covered by Medicare",
      "S" = "Non-Covered by Medicare Statute"),
    act = c(
      "A" = "Added procedure or modifier code",
      "B" = "Change in both administrative data field and long description of procedure or modifier code",
      "C" = "Change in long description of procedure or modifier code",
      "D" = "Discontinue procedure or modifier code",
      "F" = "Change in administrative data field of procedure or modifier code",
      "N" = "No maintenance for this code",
      "P" = "Payment change (MOG, pricing indicator codes, anesthesia base units,Ambulatory Surgical Centers)",
      "R" = "Re-activate discontinued/deleted procedure or modifier code",
      "S" = "Change in short description of procedure code",
      "T" = "Miscellaneous change (BETOS, type of service)"),
    price = c(
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
      "99" = "Value not established"),
    mult = c(
      "9" = "Not applicable as HCPCS not priced separately by part B (pricing indicator is 00) or value is not established (pricing indicator is 99)",
      "A" = "Not applicable as HCPCS priced under one methodology",
      "B" = "Professional component of HCPCS priced using RVUs, while technical component and global service priced by Medicare part B carriers",
      "C" = "Physician interpretation of clinical lab service is priced under physician fee schedule using RVUs, while pricing of lab service is paid under clinical lab fee schedule",
      "D" = "Service performed by physician is priced under physician fee schedule using RVUs, while service performed by clinical psychologist is priced under clinical psychologist fee schedule (not applicable as of January 1, 1998)",
      "E" = "Service performed by physician is priced under physician fee schedule using RVUs, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)",
      "F" = "Service performed by physician is priced under physician fee schedule by carriers, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)",
      "G" = "Clinical lab service priced under reasonable charge when service is submitted on claim with blood products, while service is priced under clinical lab fee schedule when there are no blood products on claim."),
    lab = c(
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
      "900" = "Clinical cytogenetics"),
    sec = c(
      "A" = "Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental",
      "B" = "Enteral and Parenteral Therapy",
      "C" = "Temporary Hospital Outpatient Prospective Payment System",
      "D" = "Dental Procedures",
      "E" = "Durable Medical Equipment",
      "G" = "Temporary Procedures & Professional Services",
      "H" = "Rehabilitative Services",
      "J" = "Drugs Administered Other Than Oral Method, Chemotherapy Drugs",
      "K" = "Temporary Codes for Durable Medical Equipment Regional Carriers",
      "L" = "Orthotic/Prosthetic Procedures",
      "M" = "Medical Services",
      "P" = "Pathology and Laboratory",
      "Q" = "Temporary Codes",
      "R" = "Diagnostic Radiology Services",
      "S" = "Private Payer Codes",
      "T" = "State Medicaid Agency Codes",
      "V" = "Vision/Hearing Services"),
    betos = c(
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
      "Z2"  = "Undefined codes"),
    tos = c(
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
      "Z" = "Third opinion on elective surgery (obsoleted 1/97)")
  )
}
