library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
rvu_xl     <- glue::glue("{root}RVU24A-010323/PPRRVU24_JAN.xlsx")

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
    mrvu_opps    = mp_used_for_opps_payment_amount) |>
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
  mutate(calc = NULL,
         cf = format(32.7442, digits = 5))

rvu$cf <- as.double(rvu$cf)

# [18,499 x 31]

rvu |>
  mutate(hcpcs_letters = str_detect(hcpcs, regex("[A-Z]"))) |>
  # mutate(level2 = str_detect(hcpcs, regex("^[A-Z]"))) |>
  # mutate(level2 = str_detect(hcpcs, regex("[A-Z]$"))) |>
  # mutate(cpt = str_detect(hcpcs, regex("^[0-9]"))) |>
  filter(hcpcs_letters == TRUE) |>
  slice(3000:5000)


# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(rvu,
                  name = "rvu",
                  title = "PFS RVU 2024",
                  description = "National Physician Fee Schedule Relative Value File January 2024 Release",
                  type = "qs")

board |> pins::write_board_manifest()
