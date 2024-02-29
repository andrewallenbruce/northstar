library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")

# NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE CALENDAR YEAR 2024
# rvu_xl <- here::here("data/RVU24A-010323/PPRRVU24_JAN.xlsx")

rvu_xl     <- glue::glue("{root}RVU24A-010323/PPRRVU24_JAN.xlsx")
pfs_pay_xl <- glue::glue("{root}PFREV24A_0/PFALL24.csv")
gpci_xl    <- glue::glue("{root}RVU24A-010323/GPCI2024.xlsx")


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
                  mp_used_for_opps_payment_amount
  ),
  readr::parse_number)) |>
  rename(
    status = status_code,
    unused = not_used_for_medicare_payment,
    work.rvu = work_rvu,
    pe.rvu_nonfac = non_fac_pe_rvu,
    nonfac_ind_na = non_fac_na_indicator,
    pe.rvu_fac = facility_pe_rvu,
    fac_ind_na = facility_na_indicator,
    mp.rvu = mp_rvu,
    total_nonfac = non_facility_total,
    total_fac = facility_total,
    pctc = pctc_ind,
    endo = endo_base,
    cf = conv_factor,
    phys_sup_diag_proc = physician_supervision_of_diagnostic_procedures,
    calc_flag = calculation_flag,
    diag_img_fm_ind = diagnostic_imaging_family_indicator,
    pe.rvu_nonfac_opps = non_facility_pe_used_for_opps_payment_amount,
    pe.rvu_fac_opps = facility_pe_used_for_opps_payment_amount,
    mp.rvu_opps = mp_used_for_opps_payment_amount
  )

rvu |>
  filter(hcpcs == "A0021")

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# pfs_pay_xl <- here::here("data/PFREV24A_0/PFALL24.csv")

pfs_pay <- readr::read_csv(pfs_pay_xl, col_types = strrep("c", 16)) |>
  slice(-c(990483:990487))

names(pfs_pay) <- c(
  "year",
  "carrier_no",
  "locality",
  "hcpcs",
  "mod",
  "fee_nonfac", # "non_fac_fee_sched_amount"
  "fee_fac", # "facility_fee_sched_amount"
  "pctc",
  "status",
  "mult_surg",
  "ther_red_nonfac", # "therapy_reduction_nonfac"
  "flat_visit", # flat_visit_fee
  "ther_red_fac", # "therapy_reduction_fac"
  "opps", # "opps_indicator"
  "opps_nonfac",
  "opps_fac"
)

pfs_pay <- pfs_pay |>
  mutate(across(c(
    year,
    contains("fac"),
    flat_visit
    ), readr::parse_number)) |>
  select(-pctc)

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
         gpci.pw_floor = x2024_pw_gpci_with_1_0_floor,
         gpci.pe = x2024_pe_gpci,
         gpci.mp = x2024_mp_gpci
  ) |>
  mutate(across(contains("gpci"), readr::parse_number)) |>
  mutate(locality_name = str_remove_all(locality_name, fixed("*")),
         state = fct(state))

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
