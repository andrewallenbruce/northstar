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
                  mp_used_for_opps_payment_amount),
                readr::parse_number))

rvu <- rvu |>
  mutate(
    non_fac_na_indicator                = ifelse(!is.na(non_fac_na_indicator), 1, 0),
    facility_na_indicator               = ifelse(!is.na(facility_na_indicator), 1, 0),
    not_used_for_medicare_payment       = ifelse(!is.na(not_used_for_medicare_payment), 1, 0),
    # mod                                 = ifelse(is.na(mod), "00", mod),
    # diagnostic_imaging_family_indicator = ifelse(diagnostic_imaging_family_indicator == "88", 1, 0),
    # non_fac_na_indicator                = as.integer(non_fac_na_indicator),
    # facility_na_indicator               = as.integer(facility_na_indicator),
    not_used_for_medicare_payment       = as.integer(not_used_for_medicare_payment),
    # mult_proc                           = as.integer(mult_proc),
    # bilat_surg                          = as.integer(bilat_surg),
    # asst_surg                           = as.integer(asst_surg),
    # co_surg                             = as.integer(co_surg),
    # team_surg                           = as.integer(team_surg)
  ) |>
  unite(rare, c("non_fac_na_indicator", "facility_na_indicator"),
        sep = "", remove = TRUE) |>
  rename(
    status      = status_code,
    # mod       = mod,
    unused      = not_used_for_medicare_payment,
    wrvu        = work_rvu,
    nprvu       = non_fac_pe_rvu,
    fprvu       = facility_pe_rvu,
    mrvu        = mp_rvu,
    ntotal      = non_facility_total,
    ftotal      = facility_total,
    pctc        = pctc_ind,
    endo        = endo_base,
    cf          = conv_factor,
    supvis      = physician_supervision_of_diagnostic_procedures,
    dximg       = diagnostic_imaging_family_indicator,
    nprvu_opps  = non_facility_pe_used_for_opps_payment_amount,
    fprvu_opps  = facility_pe_used_for_opps_payment_amount,
    mrvu_opps   = mp_used_for_opps_payment_amount,
    global      = glob_days,
    op_pre      = pre_op,
    op_intra    = intra_op,
    op_post     = post_op,
    # mult_proc = mult_proc,
    surg_bilat  = bilat_surg,
    surg_asst   = asst_surg,
    surg_co     = co_surg,
    surg_team   = team_surg) |>
  filter(!is.na(calculation_flag)) |>
  mutate(calculation_flag = NULL,
         cf = format(32.7442, digits = 5),
         ntotal = NULL,
         ftotal = NULL) |>
  mutate(op_ind = op_pre + op_intra + op_post, .before = op_pre)

rvu$cf <- as.double(rvu$cf)

rvu <- rvu |>
  select(
    hcpcs,
    description,
    mod,
    status,
    wrvu,
    nprvu,
    fprvu,
    mrvu,
    cf,
    nprvu_opps,
    fprvu_opps,
    mrvu_opps,
    global,
    op_ind,
    op_pre,
    op_intra,
    op_post,
    pctc,
    mult_proc,
    surg_bilat,
    surg_asst,
    surg_co,
    surg_team,
    endo,
    supvis,
    dximg,
    unused,
    rare
    )

# [18,499 x 28]


rvu |> select(cf) # [non_fac, facility]

rvu |>
  mutate(rare = case_match(rare,
                           "00" ~ "Neither",
                           "10" ~ "Non-Facility",
                           "01" ~ "Facility",
                           "11" ~ "Both"),
    rare = factor(rare,
                  levels = c("Neither", "Non-Facility", "Facility", "Both"))
    )

rvu |> count(supvis)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(rvu,
                  name = "rvu",
                  title = "PFS RVU 2024",
                  description = "National Physician Fee Schedule Relative Value File January 2024 Release",
                  type = "qs")

board |> pins::write_board_manifest()
