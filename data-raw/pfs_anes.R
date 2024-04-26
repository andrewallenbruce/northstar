source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

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
pin_update(
  anesthesia,
  name = "anesthesia",
  title = "Anesthesia Conversion Factor 2024",
  description = "Anesthesia Conversion Factor 2024"
)
