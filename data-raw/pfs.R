library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
pfs_pay_xl <- glue::glue("{root}PFREV24A_0/PFALL24.csv")

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# pfs_pay_xl <- here::here("data/PFREV24A_0/PFALL24.csv")

pfs_pay <- read_csv(pfs_pay_xl, col_types = strrep("c", 16)) |>
  slice(-c(990483:990487)) |>
  clean_names() |>
  select(
    year            = x2024,
    mac             = x01112,
    locality        = x05,
    hcpcs           = g0011,
    mod             = x5,
    nf_fee          = x0000030_87,
    f_fee           = x0000025_30,
    # flat_vis      = x8,
    pctc            = x0_9,
    status          = a,
    mult_surg       = x0_11,
    nther           = x0000000_00_12,
    fther           = x0000000_00_13,
    opps            = x9,
    opps_nf         = x0000000_00_15,
    opps_f          = x0000000_00_16
    )

names(pfs_pay) <- c(
  "year",
  "mac",
  "locality",
  "hcpcs",
  "mod",
  "fee_nf", # "non_fac_fee_sched_amount"
  "fee_f", # "facility_fee_sched_amount"
  "pctc",
  "mult_surg",
  "status",
  "nther", # "therapy_reduction_nonfac"
  "flat_vis", # flat_visit_fee
  "fther", # "therapy_reduction_fac"
  "opps", # "opps_indicator"
  "opps_nf",
  "opps_f"
)

pfs_pay <- pfs_pay |>
  mutate(across(c(
      year,
      contains("_fee"),
      contains("ther"),
      contains("opps_")),
    readr::parse_number)) |>
  select(-year) |>
  mutate(opps_f = if_else(opps == "9", NA_integer_, opps_f),
         opps_nf = if_else(opps == "9", NA_integer_, opps_nf))

# [990,482 x 15]

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(pfs_pay,
                  name = "pymt",
                  title = "PFS Payment Amount 2024",
                  description = "Annual Physician Fee Schedule Payment Amount File 2024",
                  type = "qs")

board |> pins::write_board_manifest()
