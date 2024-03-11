library(readxl)
library(tidyverse)
library(janitor)

root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
opps_xl <- glue::glue("{root}RVU24A-010323/OPPSCAP_JAN.xlsx")

# OPPSCAP contains the payment amounts after the application of the OPPS-based
# payment caps, except for carrier priced codes.
#
# For carrier price codes, the field only contains the OPPS-based payment caps.
#
# Carrier prices cannot exceed the OPPS-based payment caps.

opps <- read_excel(opps_xl, col_types = "text") |>
  clean_names() |>
  mutate(across(c(
    non_facilty_price,
    facility_price),
    readr::parse_number))

opps


# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(opps,
                  name = "opps",
                  title = "PFS OPPSCAP 2024",
                  description = "OPPSCAP contains the payment amounts after the application of the OPPS-based payment caps, except for carrier priced codes. For carrier price codes, the field only contains the OPPS-based payment caps. Carrier prices cannot exceed the OPPS-based payment caps.",
                  type = "qs")

board |> pins::write_board_manifest()
