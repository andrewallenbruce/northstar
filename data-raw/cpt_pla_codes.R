source(here::here("data-raw", "pins_functions.R"))
library(codexchain)

cpt_pla_codes <- codexchain::placodes |>
  select(
    hcpcs = X1,
    desc_long = X2,
    desc_medium = X3,
    desc_short = X4,
    X5,
    X6,
    date_released = X7,
    date_effective = X8,
    proprietary_name = X9,
    clinical_lab = X10,
    manufacturer = X11
  ) |>
  janitor::remove_empty()

# Update Pin
pin_update(
  cpt_pla_codes,
  name        = "cpt_pla_codes",
  title       = "Category I Proprietary Laboratory Analyses (PLA) Codes",
  description = "Category I Proprietary Laboratory Analyses (PLA) Codes"
)
