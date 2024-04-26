source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

# OPPSCAP contains the payment amounts after the application of the OPPS-based
# payment caps, except for carrier priced codes. For carrier price codes, the
# field only contains the OPPS-based payment caps. Carrier prices cannot exceed
# the OPPS-based payment caps.

opps <- read_excel(
  opps_xl,
  col_types = "text"
  ) |>
  clean_names() |>
  mutate(
    across(
      c(
    non_facilty_price,
    facility_price
    ),
    readr::parse_number
    )
  ) |>
  filter(hcpcs != "\u001a") |>
  select(
    hcpcs,
    opps_mod      = mod,
    opps_status   = procstat,
    opps_mac      = carrier,
    opps_locality = locality,
    opps_fac_amt  = facility_price,
    opps_non_amt  = non_facilty_price
    )

collapse::funique(opps$hcpcs)

# Update Pin
pin_update(
  opps,
  name        = "opps",
  title       = "PFS OPPSCAP 2024",
  description = "OPPSCAP contains the payment amounts after the application of the OPPS-based payment caps, except for carrier priced codes. For carrier price codes, the field only contains the OPPS-based payment caps. Carrier prices cannot exceed the OPPS-based payment caps."
)
