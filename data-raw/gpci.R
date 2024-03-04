library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
gpci_xl    <- glue::glue("{root}RVU24A-010323/GPCI2024.xlsx")

# ADDENDUM E. FINAL CY 2024 GEOGRAPHIC PRACTICE COST INDICES (GPCIs) BY STATE AND MEDICARE LOCALITY
# gpci_xl <- here::here("data/RVU24A-010323/GPCI2024.xlsx")

gpci <- read_excel(gpci_xl, col_types = "text") |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  filter(!is.na(state)) |>
  rename(mac      = medicare_administrative_contractor_mac,
         locality = locality_number,
         name     = locality_name,
         wgpci    = x2024_pw_gpci_with_1_0_floor,
         pgpci    = x2024_pe_gpci,
         mgpci    = x2024_mp_gpci) |>
  mutate(across(contains("gpci"), readr::parse_number)) |>
  mutate(ftnote = str_extract_all(name, fixed("*")),
         name   = str_remove_all(name, fixed("*")),
         state  = fct(state)) |>
  unnest(ftnote, keep_empty = TRUE) |>
  select(-ftnote)

# [114 x 8]
gpci

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(gpci,
                  name = "gpci",
                  title = "GPCIs 2024",
                  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024",
                  type = "qs")

board |> pins::write_board_manifest()
