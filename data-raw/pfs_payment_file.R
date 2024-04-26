source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE

pfs_pay <- read_csv(
  pay_xl,
  col_types = strrep("c", 16)
  ) |>
  slice(
    -c(990483:990483)
    ) |>
  clean_names() |>
  select(
    hcpcs = g0011,
    # pmt_year = x2024,
    pmt_mac = x01112,
    pmt_locality = x05,
    pmt_mod = x5,
    pmt_non_amt = x0000030_87,
    pmt_fac_amt = x0000025_30,
    pmt_pctc = x0_9,
    pmt_status = a,
    pmt_mult_surg = x0_11,
    pmt_therapy_reduction_non = x0000000_00_12,
    pmt_therapy_reduction_fac = x0000000_00_13,
    pmt_opps_ind = x9,
    pmt_opps_non = x0000000_00_15,
    pmt_opps_fac = x0000000_00_16
    ) |>
  mutate(
    across(c(
      contains("_amt"),
      contains("therapy"),
      contains("opps_")),
    readr::parse_number))

# [990,482 x 15]

# Update Pin
pin_update(
  pfs_pay,
  name = "pymt",
  title = "PFS Payment Amount 2024",
  description = "Annual Physician Fee Schedule Payment Amount File 2024"
)
