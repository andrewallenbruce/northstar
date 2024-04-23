library(readxl)
library(tidyverse)
library(janitor)

root   <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
rvu_xl <- glue::glue("{root}RVU24A-010323/PPRRVU24_JAN.xlsx")

# NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE CALENDAR YEAR 2024
# rvu_xl <- here::here("data/RVU24A-010323/PPRRVU24_JAN.xlsx")

rvu <- read_excel(
  rvu_xl,
  col_types = "text") |>
  row_to_names(row_number = 9) |>
  clean_names() |>
  mutate(
    across(
      c(
      work_rvu,
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
    readr::parse_number
  )) |>
  select(
    hcpcs,
    rel_desc          = description,
    rel_status        = status_code,
    rel_wrvu          = work_rvu,
    rel_mrvu          = mp_rvu,
    rel_prvu_non      = non_fac_pe_rvu,
    rel_prvu_fac      = facility_pe_rvu,
    rel_conv          = conv_factor,
    rel_non_tot       = non_facility_total,
    rel_fac_tot       = facility_total,
    rel_opps_prvu_non = non_facility_pe_used_for_opps_payment_amount,
    rel_opps_prvu_fac = facility_pe_used_for_opps_payment_amount,
    rel_opps_mrvu     = mp_used_for_opps_payment_amount,
    rel_global        = glob_days,
    rel_preop         = pre_op,
    rel_intraop       = intra_op,
    rel_postop        = post_op,
    rel_surg_bilat    = bilat_surg,
    rel_surg_asst     = asst_surg,
    rel_surg_co       = co_surg,
    rel_surg_team     = team_surg,
    rel_mult_proc     = mult_proc,
    rel_mod           = mod,
    rel_pctc          = pctc_ind,
    rel_endo          = endo_base,
    rel_dr_viz        = physician_supervision_of_diagnostic_procedures,
    rel_img_fam       = diagnostic_imaging_family_indicator,
    rel_non_na        = non_fac_na_indicator,
    rel_fac_na        = facility_na_indicator,
    rel_not_used      = not_used_for_medicare_payment,
    rel_calc_flag     = calculation_flag) |>
  mutate(rel_op_ind   = rel_preop + rel_intraop + rel_postop, .before = rel_preop)

rvu$rel_conv <- as.double(rvu$rel_conv)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    rvu,
    name = "rvu",
    title = "PFS RVU 2024",
    description = "National Physician Fee Schedule Relative Value File January 2024 Release",
    type = "qs"
  )

board |> pins::write_board_manifest()
