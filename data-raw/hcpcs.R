source(here::here("data-raw", "source_setup", "setup.R"))

url_to_scrape <- "https://www.cms.gov/medicare/coding-billing/healthcare-common-procedure-system/quarterly-update"

hcpcs_url <- rvest::read_html(url_to_scrape) |>
  rvest::html_elements("a") |>
  rvest::html_attr("href") |>
  unique() |>
  stringr::str_subset("/files/zip/")

months_re <- "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)"

hcpcs_zip_url <- dplyr::tibble(
  url = hcpcs_url,
  month = stringr::str_extract(
    url,
    stringr::regex(
      months_re,
      ignore_case = TRUE)
    ) |>
    stringr::str_to_title(),
  year = stringr::str_extract(url, "\\d{4}") |>
    as.integer(),
  dl = glue::glue("https://www.cms.gov{url}")
) |>
  dplyr::filter(
    year == substr(Sys.Date(), 1, 4),
    month == as.character(
      lubridate::month(
        lubridate::today() + lubridate::dmonths(1),
        label = TRUE,
        abbr = FALSE
        )
      )
    ) |>
  dplyr::pull(dl)

curl::multi_download(hcpcs_zip_url)

zip_paths <- fs::file_info(
  fs::dir_ls(path = here::here(),
             glob = "*.zip")) |>
  dplyr::pull(path) |>
  as.character()

# Unzip
purrr::walk(zip_paths, zip::unzip)

# Delete zips
fs::file_delete(here::here(fs::dir_ls(glob = "*.zip")))

xlsx_paths <- fs::file_info(
  fs::dir_ls(path = here::here(),
             glob = "*.xlsx")) |>
  dplyr::pull(path) |>
  as.character()

hcpcs <- xlsx_paths |>
  purrr::map(read_excel, col_types = "text") |>
  purrr::set_names(
    stringr::str_remove_all(
      janitor::make_clean_names(
        basename(xlsx_paths)), ".xlsx"))

hcpcs_jul24 <- hcpcs$hcpc2024_jul_anweb_v3 |>
  janitor::clean_names() |>
  fuimus::remove_quiet() |>
  dplyr::mutate(
    len = stringr::str_length(hcpc),
    type = dplyr::if_else(len == 2, "mod", "code"),
    len = NULL,
    add_dt = janitor::convert_to_date(add_dt),
    act_eff_dt = janitor::convert_to_date(act_eff_dt),
    term_dt = janitor::convert_to_date(term_dt),
    asc_dt = janitor::convert_to_date(asc_dt),
    .after = hcpc
    ) |>
  dplyr::filter(type == "code") |>
  dplyr::mutate(type = NULL)

# Descriptions Only
hcpcs_desc_jul24 <- hcpcs_jul24 |>
  dplyr::select(
    hcpcs_code = hcpc,
    Long = long_description,
    Short = short_description,
  ) |>
  tidyr::pivot_longer(
    cols = !hcpcs_code,
    names_to = "hcpcs_desc_type",
    values_to = "hcpcs_description"
  )

pin_update(
  hcpcs_desc_jul24,
  name = "two_descriptions",
  title = "2024 HCPCS Level II Descriptions",
  description = "2024 Healthcare Common Procedure Coding System (HCPCS)"
)

hcpcs_jul24 <- hcpcs_jul24 |>
  tidyr::unite("price", price1:price2, sep = ":", na.rm = TRUE) |>
  tidyr::unite("cim", cim1:cim2, sep = ", ", na.rm = TRUE) |>
  tidyr::unite("mcm", mcm1:mcm3, sep = ", ", na.rm = TRUE) |>
  tidyr::unite("labcert", labcert1:labcert4, sep = ", ", na.rm = TRUE) |>
  tidyr::unite("xref", xref1:xref2, sep = ", ", na.rm = TRUE) |>
  tidyr::unite("tos", tos1:tos4, sep = ":", na.rm = TRUE) |>
  dplyr::mutate(
    price = dplyr::na_if(price, ""),
    cim = dplyr::na_if(cim, ""),
    mcm = dplyr::na_if(mcm, ""),
    labcert = dplyr::na_if(labcert, ""),
    xref = dplyr::na_if(xref, ""),
    tos = dplyr::na_if(tos, "")) |>
  dplyr::select(
    hcpcs_code = hcpc,
    price,
    multi = mult_pi,
    cov,
    betos,
    tos,
    action = action_cd,
    cim,
    mcm,
    statute,
    lab = labcert,
    xref,
    asc_grp,
    proc = procnote,
    anes = anest_bu,
    date_term  = term_dt,
    date_added  = add_dt,
    date_asc = asc_dt,
    date_action = act_eff_dt
    )

pin_update(
  hcpcs_jul24,
  name = "hcpcs_lvl2",
  title = "HCPCS Level II Codes July 2024",
  description = "2024 Healthcare Common Procedure Coding System (HCPCS)"
)

hcpcs_noc_jul24 <- hcpcs$noc_codes_jul_2024 |>
  janitor::row_to_names(row_number = 2) |>
  janitor::clean_names() |>
  fuimus::remove_quiet() |>
  dplyr::mutate(
    add_date = janitor::convert_to_date(add_date),
    term_date = janitor::convert_to_date(term_date)
  )

pin_update(
  hcpcs_noc_jul24,
  name = "hcpcs_noc",
  title = "HCPCS Level II NOC Codes July 2024",
  description = "2024 Healthcare Common Procedure Coding System (HCPCS): Not Otherwise Classified Codes"
)

# Delete xlsx
fs::file_delete(here::here(fs::dir_ls(glob = "*.xlsx|*.txt")))

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
# are 5 position numeric codes representing physician and non-physician services.
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
# non-physician services that are not represented in the level I codes.


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


