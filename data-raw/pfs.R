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

# [18,500 x 31]

rvu |>
  filter(hcpcs == "A0021")

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
         wgpci = x2024_pw_gpci_with_1_0_floor,
         pgpci = x2024_pe_gpci,
         mgpci = x2024_mp_gpci
  ) |>
  mutate(across(contains("gpci"), readr::parse_number)) |>
  mutate(ftnote = str_extract_all(locality_name, fixed("*")),
         locality_name = str_remove_all(locality_name, fixed("*")),
         state = fct(state)) |>
  unnest(ftnote, keep_empty = TRUE)

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
