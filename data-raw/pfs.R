library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
rvu_xl     <- glue::glue("{root}RVU24A-010323/PPRRVU24_JAN.xlsx")
pfs_pay_xl <- glue::glue("{root}PFREV24A_0/PFALL24.csv")
gpci_xl    <- glue::glue("{root}RVU24A-010323/GPCI2024.xlsx")

# NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE CALENDAR YEAR 2024
# rvu_xl <- here::here("data/RVU24A-010323/PPRRVU24_JAN.xlsx")

rvu <- read_excel(rvu_xl, col_types = "text") |>
  row_to_names(row_number = 9) |>
  clean_names() |>
  mutate(across(c(work_rvu,
                  non_fac_pe_rvu,
                  facility_pe_rvu,
                  non_facility_total,
                  facility_total,
                  mp_rvu,
                  conv_factor,
                  pre_op,
                  intra_op,
                  post_op,
                  non_facility_pe_used_for_opps_payment_amount,
                  facility_pe_used_for_opps_payment_amount,
                  mp_used_for_opps_payment_amount), readr::parse_number)) |>
  rename(
    status       = status_code,
    notused      = not_used_for_medicare_payment,
    wrvu         = work_rvu,
    prvu_nf      = non_fac_pe_rvu,
    prvu_f       = facility_pe_rvu,
    nf_ind_na    = non_fac_na_indicator,
    f_ind_na     = facility_na_indicator,
    mrvu         = mp_rvu,
    total_nf     = non_facility_total,
    total_f      = facility_total,
    pctc         = pctc_ind,
    endo         = endo_base,
    cf           = conv_factor,
    phys_dxpx    = physician_supervision_of_diagnostic_procedures,
    calc         = calculation_flag,
    dximgfm_ind  = diagnostic_imaging_family_indicator,
    prvu_nf_opps = non_facility_pe_used_for_opps_payment_amount,
    prvu_f_opps  = facility_pe_used_for_opps_payment_amount,
    mrvu_opps    = mp_used_for_opps_payment_amount)

rvu <- rvu |>
  mutate(
    nf_ind_na   = ifelse(!is.na(nf_ind_na), 1, 0),
    f_ind_na    = ifelse(!is.na(f_ind_na), 1, 0),
    notused     = ifelse(!is.na(notused), 1, 0),
    mod         = ifelse(is.na(mod), "00", mod),
    dximgfm_ind = ifelse(dximgfm_ind == "88", 1, 0),
    nf_ind_na   = as.integer(nf_ind_na),
    f_ind_na    = as.integer(f_ind_na),
    notused     = as.integer(notused),
    mult_proc   = as.integer(mult_proc),
    bilat_surg  = as.integer(bilat_surg),
    asst_surg   = as.integer(asst_surg),
    co_surg     = as.integer(co_surg),
    team_surg   = as.integer(team_surg)
  ) |>
  rename(
    nf_rare      = nf_ind_na,
    f_rare       = f_ind_na,
    nf_total     = total_nf,
    f_total      = total_f,
    unused       = notused,
    nf_prvu      = prvu_nf,
    f_prvu       = prvu_f,
    nf_prvu_opps = prvu_nf_opps,
    f_prvu_opps  = prvu_f_opps,
    global       = glob_days,
    op_pre       = pre_op,
    op_intra     = intra_op,
    op_post      = post_op,
    surg_bilat   = bilat_surg,
    surg_asst    = asst_surg,
    surg_co      = co_surg,
    surg_team    = team_surg,
    dximg        = dximgfm_ind,
    supvis       = phys_dxpx,
  ) |>
  filter(!is.na(calc)) |>
  mutate(calc = NULL)

# [18,500 x 31]

# HCPCS Level 1
# Category I: numbers only, except for G0402, G0438, G0439
# Category II: 5 digits, ending in F
# Category III: 5 digits, ending in T
#
# HCPCS Level 2
# Level II codes are composed of a single letter [A-V], followed by 4 digits.
# A = Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental
lk_lvl2 <- c(
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
  "V" = "Vision/Hearing Services"
)

rvu |>
  mutate(hcpcs_letters = str_detect(hcpcs, regex("[A-Z]"))) |>
  # mutate(level2 = str_detect(hcpcs, regex("^[A-Z]"))) |>
  # mutate(level2 = str_detect(hcpcs, regex("[A-Z]$"))) |>
  # mutate(cpt = str_detect(hcpcs, regex("^[0-9]"))) |>
  filter(hcpcs_letters == TRUE) |>
  slice(3000:5000)
  glimpse()

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# pfs_pay_xl <- here::here("data/PFREV24A_0/PFALL24.csv")

pfs_pay <- readr::read_csv(pfs_pay_xl,
                           col_types = strrep("c", 16)) |>
  slice(-c(990483:990487))

names(pfs_pay) <- c(
  "year",
  "mac",
  "locality",
  "hcpcs",
  "mod",
  "fee_nf", # "non_fac_fee_sched_amount"
  "fee_f", # "facility_fee_sched_amount"
  "pctc",
  "status",
  "mult_surg",
  "therapy_nf", # "therapy_reduction_nonfac"
  "flatfee_visit", # flat_visit_fee
  "therapy_f", # "therapy_reduction_fac"
  "opps", # "opps_indicator"
  "opps_nf",
  "opps_f"
)

pfs_pay <- pfs_pay |>
  mutate(across(c(
    year,
    contains("fee"),
    contains("therapy"),
    contains("opps_"),
    ), readr::parse_number)) |>
  select(-pctc)

# [990,482 x 15]
pfs_pay |>
  filter(hcpcs == "A0021")

# ADDENDUM E. FINAL CY 2024 GEOGRAPHIC PRACTICE COST INDICES (GPCIs) BY STATE AND MEDICARE LOCALITY
# gpci_xl <- here::here("data/RVU24A-010323/GPCI2024.xlsx")

gpci <- read_excel(gpci_xl, col_types = "text") |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  filter(!is.na(state)) |>
  rename(mac = medicare_administrative_contractor_mac,
         locality = locality_number,
         name = locality_name,
         wgpci = x2024_pw_gpci_with_1_0_floor,
         pgpci = x2024_pe_gpci,
         mgpci = x2024_mp_gpci) |>
  mutate(across(contains("gpci"), readr::parse_number)) |>
  mutate(ftnote = str_extract_all(name, fixed("*")),
         name = str_remove_all(name, fixed("*")),
         state = fct(state)) |>
  unnest(ftnote, keep_empty = TRUE) |>
  select(-ftnote)

# [114 x 8]
gpci


board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(rvu,
                  name = "rvu",
                  title = "PFS RVU 2024",
                  description = "National Physician Fee Schedule Relative Value File January 2024 Release",
                  type = "qs")
board |>
  pins::pin_write(pfs_pay,
                  name = "pymt",
                  title = "PFS Payment Amount 2024",
                  description = "Annual Physician Fee Schedule Payment Amount File 2024",
                  type = "qs")
board |>
  pins::pin_write(gpci,
                  name = "gpci",
                  title = "GPCIs 2024",
                  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024",
                  type = "qs")

board |> pins::write_board_manifest()
