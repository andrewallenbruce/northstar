library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
pfs_pay_xl <- glue::glue("{root}PFREV24A_0/PFALL24.csv")

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# pfs_pay_xl <- here::here("data/PFREV24A_0/PFALL24.csv")

pfs_pay <- read_csv(pfs_pay_xl, col_types = strrep("c", 16)) |>
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
  "mult_surg",
  "status",
  "ther_nf", # "therapy_reduction_nonfac"
  "flat_vis", # flat_visit_fee
  "ther_f", # "therapy_reduction_fac"
  "opps", # "opps_indicator"
  "opps_nf",
  "opps_f"
)

pfs_pay <- pfs_pay |>
  mutate(across(
    c(
      year,
      contains("fee"),
      contains("ther_"),
      contains("opps_"),
      flat_vis
    ),
    readr::parse_number)) |>
  select(-pctc) |>
  mutate(mod = ifelse(is.na(mod), "00", mod),
         opps = ifelse(opps == "9", "0", opps))

# [990,482 x 15]
pfs_pay <- pfs_pay |>
  select(-c(year, fee_nf, fee_f, opps_nf, opps_f)) |>
  select(mac,
         locality,
         hcpcs,
         mod,
         status,
         mult_surg,
         flat_vis,
         ther_nf,
         ther_f,
         opps
         )

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(pfs_pay,
                  name = "pymt",
                  title = "PFS Payment Amount 2024",
                  description = "Annual Physician Fee Schedule Payment Amount File 2024",
                  type = "qs")

board |> pins::write_board_manifest()
