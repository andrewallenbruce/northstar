source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))

# ADDENDUM E. FINAL CY 2024 GEOGRAPHIC PRACTICE COST INDICES (GPCIs) BY STATE AND MEDICARE LOCALITY
# https://www.ama-assn.org/system/files/geographic-practice-cost-indices-gpcis.pdf

gpci <- read_excel(
  gpci_xl,
  col_types = "text"
  ) |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  filter(!is.na(state)) |>
  rename(
    mac = medicare_administrative_contractor_mac,
    locality = locality_number,
    area = locality_name,
    gpci_work = x2024_pw_gpci_with_1_0_floor,
    gpci_pe = x2024_pe_gpci,
    gpci_mp = x2024_mp_gpci) |>
  mutate(
    across(
      c(gpci_work, gpci_pe, gpci_mp),
      readr::parse_number
      )
    ) |>
  mutate(
    ftnote = str_extract_all(area, fixed("*")),
    area = str_remove_all(area, fixed("*"))) |>
  unnest(ftnote, keep_empty = TRUE) |>
  select(-ftnote)

gpci

locco <- read_excel(
  locco_xl,
  col_types = "text"
  ) |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  filter(!is.na(counties)) |>
  fill(state) |>
  rename(
    mac = medicare_adminstrative_contractor,
    locality = locality_number) |>
  # *  Payment locality is serviced by two carriers.
  mutate(two_macs = str_extract_all(fee_schedule_area, fixed("*")),
         fee_schedule_area = str_remove_all(fee_schedule_area, fixed("*"))) |>
  unnest(two_macs, keep_empty = TRUE) |>
  mutate(two_macs = if_else(is.na(two_macs), FALSE, TRUE))

states <- dplyr::tibble(
  abb = gpci |> count(state) |> pull(state),
  full = locco |> count(state) |> pull(state)
)

states[1, 2] <- "ALASKA"
states[2, 2] <- "ALABAMA"
states[8, 2] <- "DISTRICT OF COLUMBIA"
states[9, 2] <- "DELAWARE"
states[48, 2] <- "VIRGINIA"
states[49, 2] <- "VIRGIN ISLANDS"

states <- states |> tibble::deframe()

locco <- locco |>
  mutate(state = fct_recode(state, !!!states),
         state = as.character(state))

gpci <- gpci |>
  left_join(locco) |>
  select(-fee_schedule_area, -two_macs) |>
  mutate(counties = if_else(is.na(counties), "ALL COUNTIES", counties)) |>
  select(
    mac,
    state,
    locality,
    area,
    counties,
    dplyr::everything()
  )

# Update Pin
pin_update(
  gpci,
  name = "gpci",
  title = "GPCIs 2024",
  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024"
)
