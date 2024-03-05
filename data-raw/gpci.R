library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
gpci_xl    <- glue::glue("{root}RVU24A-010323/GPCI2024.xlsx")
locco_xl    <- glue::glue("{root}RVU24A-010323/24LOCCO.xlsx")

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


locco <- read_excel(locco_xl, col_types = "text") |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  filter(!is.na(counties)) |>
  fill(state) |>
  rename(mac = medicare_adminstrative_contractor,
         locality = locality_number) |>
  # *  Payment locality is serviced by two carriers.
  mutate(two_macs = str_extract_all(fee_schedule_area, fixed("*")),
         fee_schedule_area = str_remove_all(fee_schedule_area, fixed("*"))) |>
  unnest(two_macs, keep_empty = TRUE) |>
  mutate(two_macs = if_else(is.na(two_macs), FALSE, TRUE))


states <- dplyr::tibble(
  abb = gpci() |> count(state) |> pull(state),
  full = locco |> count(state) |> pull(state)
)

states |> print(n = Inf)

states[8, 2] <- "DISTRICT OF COLUMBIA"
states[9, 2] <- "DELAWARE"
states[48, 2] <- "VIRGINIA"
states[49, 2] <- "VIRGIN ISLANDS"

states <- states |> tibble::deframe()

locco <- locco |>
  mutate(state = fct_recode(state, !!!states))

gpci <- gpci |>
  left_join(locco,
            by = join_by(mac, state, locality),
            relationship = "many-to-many") |>
  select(-fee_schedule_area)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(gpci,
                  name = "gpci",
                  title = "GPCIs 2024",
                  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024",
                  type = "qs")

board |> pins::write_board_manifest()
