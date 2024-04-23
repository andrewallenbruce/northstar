library(readxl)
library(tidyverse)
library(janitor)

# level2    <- "C:/Users/Andrew/Desktop/payer_guidelines/data/HCPC2024_JAN_ANWEB_v4/HCPC2024_JAN_ANWEB_v4.xlsx" # v4
# noc_codes   <- glue::glue("{hcpcs}NOC codes_APR 2024.xlsx")
# transreport <- glue::glue("{hcpcs}HCPC2024_APR_Transreport_ANWEB_v5.xlsx")
# corrections <- glue::glue("{hcpcs}HCPC2024_APR_Corrections_to_v5.xlsx")

hcpcs       <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/HCPC2024_APR_ANWEB_v5/")
level2      <- glue::glue("{hcpcs}HCPC2024_APR_ANWEB_v5.xlsx") # v5
procnotes   <- glue::glue("{hcpcs}proc_notes_APR2024.txt")

proc_note <- readr::read_lines(procnotes)[4:602]

proc_note <- vctrs::vec_c(
  proc_note[1:359],
  proc_note[361:599]
) |>
  stringr::str_remove_all("\\t") |>
  stringr::str_remove_all(stringr::fixed("*")) |>
  stringr::str_squish() |>
  dplyr::na_if("")

proc_note <- proc_note[!is.na(proc_note)]

proc_note <- dplyr::tibble(note = proc_note) |>
  tidyr::separate_wider_delim(
    cols = note,
    delim = "--",
    names = c("note", "desc"),
    too_few = "align_end"
  ) |>
  tidyr::fill(note)

proc_note <- proc_note |>
  dplyr::mutate(id = dplyr::consecutive_id(note)) |>
  dplyr::group_by(id) |>
  tidyr::nest(strings = c(desc)) |>
  dplyr::rowwise() |>
  dplyr::mutate(description = purrr::map(strings, ~paste0(., collapse = " "))) |>
  tidyr::unnest(cols = c(description)) |>
  dplyr::ungroup() |>
  dplyr::select(note, description) |>
  dplyr::mutate(
    description = stringr::str_replace_all(description, '"', " "),
    description = stringr::str_squish(description),
    delete = stringr::str_detect(description, "THIS PROCESSING NOTE DELETED"),
    date_deleted = dplyr::if_else(delete, stringr::str_extract(
      description, stringr::regex("[0-9]{1}/[0-9]{1}/[0-9]{2}")), NA_character_),
    date_deleted = clock::date_parse(date_deleted, format = "%1m/%1d/%y"),
    delete = NULL)

# correct <- read_excel(corrections, col_types = "text") |>
#   clean_names() |>
#   remove_empty(which = c("rows", "cols")) |>
#   mutate(effective_date = excel_numeric_to_date(as.numeric(effective_date))) |>
#   select(hcpcs = hcpcs_mod_code,
#          lvlII_action = action,
#          lvlII_desc_short = short_description,
#          lvlII_desc_long = long_description,
#          lvlII_date_eff = effective_date,
#          lvlII_tos = tos,
#          lvlII_betos = betos,
#          lvlII_coverage = coverage,
#          lvlII_price = pricing,
#          )
#
# trans <- read_excel(transreport, col_types = "text") |>
#   clean_names() |>
#   remove_empty(which = c("rows", "cols")) |>
#   select(hcpcs = hcpc,
#          lvlII_action = action_cd,
#          lvlII_desc_short = short_description,
#          lvlII_desc_long = long_description)
#
# noc <- read_excel(noc_codes,
#                   col_types = "text") |>
#   row_to_names(2) |>
#   clean_names() |>
#   remove_empty(which = c("rows", "cols")) |>
#   mutate(add_date = convert_to_date(add_date),
#          term_date = convert_to_date(term_date)) |>
#   select(hcpcs,
#          lvlII_desc_long = long_description,
#          lvlII_date_added = add_date,
#          lvlII_date_term = term_date)

two <- read_excel(level2, col_types = "text") |>
  clean_names() |>
  remove_empty(which = c("rows", "cols")) |>
  rename(hcpcs = hcpc) |>
  mutate(len = str_length(hcpcs), .after = hcpcs) |>
  mutate(type = if_else(len == 2, "mod", "code"), .after = hcpcs) |>
  mutate(len = NULL) |>
  mutate(add_dt = convert_to_date(add_dt),
         act_eff_dt = convert_to_date(act_eff_dt),
         term_dt = convert_to_date(term_dt),
         asc_dt = convert_to_date(asc_dt)) |>
  unite("price", price1:price2, sep = ":", remove = TRUE, na.rm = TRUE) |>
  unite("cim", cim1:cim2, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("mcm", mcm1:mcm3, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("labcert", labcert1:labcert4, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("xref", xref1:xref2, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("tos", tos1:tos4, sep = ":", remove = TRUE, na.rm = TRUE) |>
  mutate(price = na_if(price, "")) |>
  mutate(cim = na_if(cim, "")) |>
  mutate(mcm = na_if(mcm, "")) |>
  mutate(labcert = na_if(labcert, "")) |>
  mutate(xref = na_if(xref, "")) |>
  mutate(tos = na_if(tos, "")) |>
  rename(lvl2_date_added  = add_dt,
         lvl2_action_date = act_eff_dt,
         lvl2_date_terminated  = term_dt,
         lvl2_action = action_cd,
         lvl2_desc_short = short_description,
         lvl2_desc_long = long_description,
         lvl2_asc = asc_grp,
         lvl2_coverage = cov,
         lvl2_multi_price = mult_pi)

# hcpcs
#
# The Healthcare Common Prodecure Coding System (HCPCS) is a collection of
# codes that represent procedures, supplies, products and services which may be
# provided to Medicare beneficiaries and to individuals enrolled in private
# health insurance programs. The codes are divided into two levels, or groups,
# as described Below:
#
# Level I Codes and descriptors copyrighted by the American Medical
# Association's current procedural terminology, fourth # edition (CPT-4). These
# are 5 position numeric codes representing physician and nonphysician services.
#
# **** NOTE: ****
# CPT-4 codes including both long and short descriptions shall be used in
# accordance with the CMS/AMA agreement. Any other use violates the AMA
# copyright.
#
# Level II Includes codes and descriptors copyrighted by the American Dental
# Association's current dental terminology, (CDT-2023). These are 5 position
# alpha-numeric codes comprising the d series.
#
# All level II codes and descriptors are approved and maintained jointly by the
# alpha-numeric editorial panel (consisting of CMS, the Health Insurance
# Association of America, and the Blue Cross and Blue Shield Association).
#
# These are 5 position alpha- numeric codes representing primarily items and
# nonphysician services that are not represented in the level I codes.


# type == "mod"
#
# A modifier provides the means by which the reporting physician or provider can
# indicate that a service or procedure that has been performed has been altered
# by some specific circumstance but not changed in its definition or code. The
# judicious application of modifiers obviates the necessity for separate
# procedure listings that may describe the modifying circumstance. Modifiers may
# be used to indicate to the recipient of a report that:
#
#    * A service or procedure has both a professional and technical component.
#    * A service or procedure was performed by more than one physician and/or in more than one location.
#    * A service or procedure has been increased or reduced.
#    * Only part of a service was performed.
#    * An adjunctive service was performed.
#    * A bilateral procedure was performed.
#    * A service or procedure was provided more than once.
#    * Unusual events occurred.
#
# HCPCS modifier codes are divided into two levels, or groups, as described
# below:
#
# Level I Codes and descriptors copyrighted by the American Medical
# Association's current procedural terminology, fourth edition (CPT-4). These
# are 2 position numeric codes.
#
# **** NOTE: ****
# CPT-4 codes including long, short and consumer friendly descriptions shall be
# used in accordance with the CMS/AMA agreement. Any other use violates the AMA
# copyright.
#
# Level II Codes and descriptors approved and maintained jointly by the
# alpha-numeric editorial panel (consisting of CMS, the Health Insurance
# Association of America, and the Blue Cross and Blue Shield Association). These
# are 2 position alpha-numeric codes.


# seqnum
#
# HCPCS Sequence Number. Sequence number by 100s. Used to group procedure or
# modifier codes together.

# recid
#
# HCPCS Record Identification Code.
#
# 3 = First line of procedure record also contains detail information in
# positions 92-275
#
# 4 = Second, third, fourth, etc., Description of procedure record.  No detail
# information in positions 92-275
#
# 7 = First line of modifier record also contains detail information in
# positions 92-275
#
# 8 = Second, third, fourth, etc., Description of modifier record.  No detail
# information in positions 92-275

# desc_long
#
# HCPCS Long Description. Contains all text of procedure or modifier long
# descriptions. As of 2013, this field contains the consumer friendly
# descriptions for the AMA CPT codes. The AMA owns the copyright on the CPT
# codes and descriptions; CPT codes and descriptions are not public property and
# must always be used in compliance with copyright law.

# desc_short
#
# HCPCS Short Description. Short descriptive text of procedure or modifier code
# (28 characters or less). The AMA owns the copyright on the CPT codes and
# descriptions; CPT codes and descriptions are not public property and must
# always be used in compliance with copyright law.

# price
#
# HCPCS Pricing Indicator Code.  Code used to identify the appropriate
# methodology for developing unique pricing amounts under part B. A procedure
# may have one to four pricing codes.
#
# 00 = Service not separately priced by part B (e.G., services not covered,
# bundled, used by part a only, etc.)
#
# Physician Fee Schedule And Non-Physician Practitioners Linked To The Physician
# Fee Schedule
# -----------------------------------------------------------------------------
# 11 = Price established using national rvu's 12 = Price established using
# national anesthesia base units 13 = Price established by carriers (e.G., not
# otherwise classified, individual determination, carrier discretion)
#
# Clinical Lab Fee Schedule
# -------------------------
# 21 = Price subject to national limitation amount 22 = Price established by
# carriers (e.G., gap-fills, carrier established panels)
#
# Durable Medical Equipment, Prosthetics, Orthotics, Supplies And Surgical
# Dressings
# ------------------------------------------------------------------------------
# 31 = Frequently serviced DME (price subject to floors and ceilings)
# 32 = Inexpensive & routinely purchased DME (price subject to floors and ceilings)
# 33 = Oxygen and oxygen equipment (price subject to floors and ceilings)
# 34 = DME supplies (price subject to floors and ceilings)
# 35 = Surgical dressings (price subject to floors and ceilings)
# 36 = Capped rental DME (price subject to floors and ceilings)
# 37 = Ostomy, tracheostomy and urological supplies (price subject to floors and ceilings)
# 38 = Orthotics, prosthetics, prosthetic devices & vision services (price subject to floors and ceilings)
# 39 = Parenteral and Enteral Nutrition
# 40 = Lymphedema Compression Treatment Items (eff 1/1/2024)
# 45 = Customized DME items
# 46 = Carrier priced (e.g., not otherwise classified, individual determination, carrier discretion, gap-filled amounts)
#
# Other
# -----
# 51 = Drugs 52 = Reasonable charge 53 = Statute 54 = Vaccinations 55 = Splints
# and Casts (effec 10/1/2014) 56 = IOL's inserted in a physician's office (eff
# 10/1/2014) 57 = Other carrier priced 99 = Value not established

# multi_price
#
# HCPCS Multiple Pricing Indicator Code. Code used to identify instances where a
# procedure could be priced under multiple methodologies.
#
# 9 = Not applicable as HCPCS not priced separately by part B (pricing indicator is 00) or value is not established (pricing indicator is '99')
# A = Not applicable as HCPCS priced under one methodology
# B = Professional component of HCPCS priced using RVU's, while technical component and global service priced by Medicare part B carriers
# C = Physician interpretation of clinical lab service is priced under physician fee schedule using RVU's, while pricing of lab service is paid under clinical lab fee schedule
# D = Service performed by physician is priced under physician fee schedule using RVU's, while service performed by clinical psychologist is priced under clinical psychologist fee schedule (not applicable as of January 1, 1998)
# E = Service performed by physician is priced under physician fee schedule using RVU's, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)
# F = Service performed by physician is priced under physician fee schedule by carriers, service performed by clinical psychologist is priced under clinical psychologist's fee schedule and service performed by clinical social worker is priced under clinical social worker fee schedule (not applicable as of January 1, 1998)
# G = Clinical lab service priced under reasonable charge when service is submitted on claim with blood products, while service is priced under clinical lab fee schedule when there are no blood products on claim.

# cim
#
# HCPCS Coverage Issues Manual Reference Section Number. Number identifying the
# reference section of the coverage issues manual.

# mcm
#
# HCPCS Medicare Carriers Manual Reference Section Number. Number identifying a
# section of the Medicare carriers manual.

# statute
#
# HCPCS Statute Number. Number identifying statute reference for coverage or
# noncoverage of procedure or service.

# labcert
#
# HCPCS Lab Certification Code. Code used to classify laboratory procedures
# according to the specialty certification categories listed by CMS. Any
# generally certified laboratory (e.g., 100) may perform any of the tests in its
# subgroups (e.g., 110, 120, etc.).
#
# 010 = Histocompatibility testing
# 100 = Microbiology
#       110 = Bacteriology
#       115 = Mycobacteriology
#       120 = Mycology
#       130 = Parasitology
#       140 = Virology
#       150 = Other microbiology
# 200 = Diagnostic immunology
#       210 = Syphilis serology
#       220 = General immunology
# 300 = Chemistry
#       310 = Routine chemistry
#       320 = Urinalysis
#       330 = Endocrinology
#       340 = Toxicology
#       350 = Other chemistry
# 400 = Hematology
# 500 = Immunohematology
#       510 = Abo group & RH type
#       520 = Antibody detection (transfusion)
#       530 = Antibody detection (nontransfusion)
#       540 = Antibody identification
#       550 = Compatibility testing
#       560 = Other immunohematology
# 600 = Pathology
#       610 = Histopathology
#       620 = Oral pathology
#       630 = Cytology
# 800 = Radiobioassay
# 900 = Clinical cytogenetics

dplyr::tibble(
  labcert = c("010", 100, 110, 115, 120, "110, 120, 130, 400")
  ) |>
  case_labcert(labcert)


# xref
#
# HCPCS Cross Reference Code. An explicit reference crosswalking a deleted code
# or a code that is not valid for Medicare to a valid current code (or range of
# codes).

# coverage
#
# HCPCS Coverage Code. A code denoting Medicare coverage status.
#
# C = Carrier judgment
# D = Special coverage instructions apply
# I = Not payable by Medicare
# M = Non-covered by Medicare
# S = Non-covered by Medicare statute

# asc_grp
#
# HCPCS ASC Payment Group Code. The 'YY' indicator represents that this
# procedure is approved to be performed in an ambulatory surgical center. You
# must access the ASC tables on the mainframe or CMS website to get the dollar
# amounts.
#
# EDIT RULES :
#    CODE: YY
#    BLANK = Not Approved For ASC

# ????
#
# HCPCS MOG Payment Group Code. Medicare outpatient groups (MOG) payment group
# code.
#
# COMMENTS :
#    * 1st digit indicates the body system
#    * 2nd digit is sequential numbering within the body system
#    * 3rd digit is the level of intensity where:
#        * '1', '2', '3' or '4' represents levels for a given group type
#        * '0' and '9' represent single level for a given group type
#
# 000 = No MOG applies
#
# Integumentary
# -------------
# 102 = Level II needle biopsy/aspiration
# 112 = Level II incision and drainage
# 132 = Level II debridement/destruction
# 142 = Level II excision/biopsy
# 143 = Level III excision/biopsy
# 151 = Level I skin repair
# 152 = Level II skin repair
# 153 = Level III skin repair
# 160 = Incision/excision breast
# 169 = Breast reconstruction/mastectomy
#
# Musculoskeletal
# ---------------
# 201 = Level I skull and facial bone procedures
# 202 = Level II skull and facial bone procedures
# 211 = Level I hand musculoskeletal procedures
# 212 = Level II hand musculoskeletal procedures
# 221 = Level I foot musculoskeletal procedures
# 222 = Level II foot musculoskeletal procedures
# 231 = Level I musculoskeletal procedures
# 232 = Level II musculoskeletal procedures
# 233 = Level III musculoskeletal procedures
# 241 = Level I arthroscopy
# 242 = Level II arthroscopy
# 260 = Closed treatment fracture finger/toe/trunk
# 269 = Closed treatment fracture/dislocation/except finger/toe/trunk
# 270 = Open/percutaneous treatment fracture or dislocation
# 279 = Bone/joint manipulation under anesthesia
# 280 = Bunion procedures
# 289 = Arthroplasty
# 290 = Arthroplasty with prosthesis
#
# ENT/Respiratory/Cardiovascular/Lymphatic/Endocrine
# -------------------------------------------------
# 302 = Level II ENT procedures
# 303 = Level III ENT procedures
# 304 = Level IV ENT procedures
# 309 = Implantation of cochlear device (ASC rate does not include cost of implant)
# 310 = Nasal cauterization/packing
# 319 = Tonsil/adenoid procedures
# 322 = Level II endoscopy upper airway
# 323 = Level III endoscopy upper airway
# 329 = Endoscopy lower airway
# 330 = Thoracentesis/lavage procedures
# 350 = Placement transvenous caths/cutdown
# 359 = Removal/revision, pacemaker/vascular device
# 360 = Vascular ligation
# 369 = Vascular repair/fistula construction
# 370 = Lymph node excisions
# 379 = Thyroid/lymphadenectomy procedures
#
# Digestive
# --------
# 400 = Esophageal dilation without endoscopy
# 410 = Esophagoscopy
# 421 = Level I upper GI endoscopy/intubation
# 422 = Level II upper GI endoscopy/intubation
# 429 = Lower GI endoscopy
# 430 = Anoscopy and diagnostic sigmoidoscopy
# 439 = Therapeutic proctosigmoidoscopy
# 440 = Small intestine endoscopy
# 449 = Percutaneous biliary endoscopic procedures
# 450 = Endoscopic retrograde cholangio-pancreatography (ERCP)
# 460 = Hernia/hydrocele procedures
# 472 = Level II anal/rectal procedures
# 473 = Level III anal/rectal procedures
# 480 = Peritoneal and abdominal procedures
# 490 = Tube procedures
#
# Urinary/Genital
# --------------
# 501 = Level I laparoscopy
# 502 = Level II laparoscopy
# 509 = Lithotripsy
# 511 = Level I cystourethroscopy and other genitourinary procedures
# 512 = Level II cystourethroscopy and other genitourinary procedures
# 513 = Level III cystourethroscopy and other genitourinary procedures
# 521 = Level I urethral procedures
# 522 = Level II urethral procedures
# 530 = Circumcision
# 539 = Penile procedures
# 540 = Insertion of penile prosthesis (ASC rate does include cost of implant)
# 549 = Testes/epididymis procedures
# 550 = Prostrate biopsy
# 562 = Level II female reproductive procedures
# 563 = Level III female reproductive procedures
# 570 = Surgical hysteroscopy
# 579 = D & C
# 580 = Spontaneous abortion
# 589 = Therapeutic abortion
#
# Nervous/Eye
# ----------
# 602 = Level II nervous system injections
# 609 = Revision/removal neurological device
# 610 = Implantation of neurostimulator electrodes (ASC rate does not include cost of implant)
# 619 = Implantation of neurological devices (ASC rate does not include cost of implant)
# 621 = Level I nerve procedures
# 622 = Level II nerve procedures
# 629 = Spinal tap
# 639 = Laser eye procedures except retinal
# 640 = Cataract procedures
# 649 = Cataract procedures with IOL insert (includes $150 insert)
# 651 = Level I anterior segment eye procedures
# 652 = Level II anterior segment eye procedures
# 659 = Corneal transplant (ASC rate includes price of transplant)
# 660 = Posterior segment eye procedures
# 669 = Strabismus/muscle procedures
# 673 = Level III eye procedure
# 674 = Level IV eye procedure
# 680 = Vitrectomy
# 689 = Implantation/replacement of intraitreal drug


# ???
#
# HCPCS MOG Payment Policy Indicator. Indicator identifying whether a HCPCS code
# is subject to payment of an ASC facility fee, to a separate fee under another
# provision of Medicare, or to no fee at all.
#
# 1 = ASC covered procedure
# 2 = Bundled service/no separate payment
# 3 = Excluded from ASC list
# 4 = Invalid code/90 day grace period
# 6 = Separate payment when furnished in an ASC
# 7 = ASC restricted coverage procedure
# 9 = ASC payment not applicable

# ???
#
# HCPCS MOG Effective Date. The date the procedure is assigned to the Medicare
# outpatient group (MOG) payment group.

# procnote
#
# HCPCS Processing Note Number. Number identifying the processing note contained
# in Appendix A of the HCPCS manual.

# betos
#
# HCPCS Berenson-Eggers Type Of Service Code. This field is valid beginning with
# 2003 data. The Berenson-Eggers Type of Service (BETOS) for the procedure code
# based on generally agreed upon clinically meaningful groupings of procedures
# and services.
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


# tos
#
# HCPCS Type Of Service Code. The carrier assigned CMS type of service which
# describes the particular kind(s) of service represented by the procedure code.

# anest_bu
#
# HCPCS Anesthesia Base Unit Quantity. The base unit represents the level of
# intensity for anesthesia procedure services that reflects all activities
# except time. These activities include usual preoperative and post-operative
# visits, the administration of fluids and/or blood incident to anesthesia care,
# and monitering procedures. (Note: the payment amount for anesthesia services
# is based on a calculation using base unit, time units, and the conversion
# factor.)

# date_added
#
# HCPCS Code Added Date. The year the HCPCS code was added to the Healthcare
# common procedure coding system.

# action_date
#
# HCPCS Action Effective Date. Effective date of action to a procedure or
# modifier code

# date_terminated
#
# HCPCS Termination Date. Last date for which a procedure or modifier code may
# be used by Medicare providers.

# action
#
# HCPCS Action Code. A code denoting the change made to a procedure or modifier
# code within the HCPCS system.
#
# A = Add procedure or modifier code
# B = Change in both administrative data field and long description of procedure or modifier code
# C = Change in long description of procedure or modifier code
# D = Discontinue procedure or modifier code
# F = Change in administrative data field of procedure or modifier code
# N = No maintenance for this code
# P = Payment change (MOG, pricing indicator codes, anesthesia base units,Ambulatory Surgical Centers)
# R = Re-activate discontinued/deleted procedure  or modifier code
# S = Change in short description of procedure code
# T = Miscellaneous change (BETOS, type of service)
action_cd <- function() {
  c(
    "A" = "A: Code added",
    "B" = "B: Code change - Admin data, Long description",
    "C" = "C: Code change - Long description",
    "D" = "D: Code Discontinued",
    "F" = "F: Code change - Admin data",
    "N" = "N: No maintenance for code",
    "P" = "P: Payment change",
    "R" = "R: Code reactivated",
    "S" = "S: Code change - Short description",
    "T" = "T: Miscellaneous change"
  )
}


two <- two |>
  select(hcpcs,
         lvlII_type        = type,
         lvlII_desc_long   = lvl2_desc_long,
         lvlII_desc_short  = lvl2_desc_short,
         lvlII_date_added  = lvl2_date_added,
         lvlII_date_term   = lvl2_date_terminated,
         lvlII_seqnum      = seqnum,
         lvlII_recid       = recid,
         lvlII_price       = price,
         lvlII_mprice      = lvl2_multi_price,
         lvlII_cim         = cim,
         lvlII_mcm         = mcm,
         lvlII_statute     = statute,
         lvlII_labcert     = labcert,
         lvlII_xref        = xref,
         lvlII_coverage    = lvl2_coverage,
         lvlII_asc_group   = lvl2_asc,
         lvlII_asc_date    = asc_dt,
         lvlII_procnote    = procnote,
         lvlII_betos       = betos,
         lvlII_tos         = tos,
         lvlII_anesths     = anest_bu,
         lvlII_action_date = lvl2_action_date,
         lvlII_action      = lvl2_action)


level_two <- list(
  two     = two,
  correct = correct,
  trans   = trans,
  noc     = noc,
  proc    = proc_note
)

x <- search_level_two()

proc <- x$proc |>
  dplyr::select(
    lvlII_procnote = note,
    lvlII_procnote_desc = description,
    lvlII_procnote_date_deleted = date_deleted
  )

# Split off the modifiers?

# Join with proc and delete

two <- x$two |>
  dplyr::left_join(
    proc,
    by = dplyr::join_by(lvlII_procnote)) |>
  dplyr::select(
    hcpcs,
    lvlII_type,
    lvlII_desc_short,
    lvlII_desc_long,
    lvlII_date_added,
    lvlII_date_term,
    lvlII_price,
    lvlII_mprice,
    lvlII_labcert,
    lvlII_xref,
    lvlII_coverage,
    lvlII_tos,
    lvlII_betos,
    lvlII_procnote,
    lvlII_procnote_desc,
    lvlII_procnote_date_deleted,
    lvlII_seqnum,
    lvlII_recid,
    lvlII_cim,
    lvlII_mcm,
    lvlII_statute,
    lvlII_asc_group,
    lvlII_asc_date,
    lvlII_anesths,
    lvlII_action_date,
    lvlII_action
  ) |>
  janitor::remove_empty(which = c("rows", "cols"))


board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(two,
                  name = "level_two",
                  title = "2024 HCPCS Level II Update",
                  description = "2024 Healthcare Common Procedure Coding System (HCPCS)",
                  type = "qs")

board |> pins::write_board_manifest()
