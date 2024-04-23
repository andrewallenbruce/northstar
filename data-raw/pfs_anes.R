library(readxl)
library(tidyverse)
library(janitor)

root     <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")
anes_xl  <- glue::glue("{root}RVU24A-010323/ANES2024.xlsx")

anesthesia <- read_excel(
  anes_xl,
  col_types = "text"
) |>
  clean_names() |>
  mutate(
    contractor = stringr::str_pad(contractor, 5, pad = "0", side = "left"),
    locality = stringr::str_pad(locality, 2, pad = "0", side = "left"),
    x2024_anesthesia_conversion_factor = as.double(x2024_anesthesia_conversion_factor),
    work = as.numeric(work),
    pe = as.numeric(pe),
    mp = as.numeric(mp)
    ) |>
  select(
    anes_mac         = contractor,
    anes_locality    = locality,
    anes_area        = locality_name,
    anes_conv        = x2024_anesthesia_conversion_factor,
    anes_work        = work,
    anes_practice    = pe,
    anes_malpractice = mp
  )


# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    anesthesia,
    name = "anesthesia",
    title = "Anesthesia Conversion Factor 2024",
    description = "Anesthesia Conversion Factor 2024",
    type = "qs")

board |> pins::write_board_manifest()
