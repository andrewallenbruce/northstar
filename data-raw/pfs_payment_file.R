source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# [990,482 x 15]
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

# MAC, Locality, Non-Facility Fee, Facility Fee
# A tibble: 990,485 × 5
pay_mac_fee <- pfs_pay |>
  dplyr::select(
    hcpcs,
    mac = pmt_mac,
    locality = pmt_locality,
    non_fee = pmt_non_amt,
    fac_fee = pmt_fac_amt
  )

pin_update(
  pay_mac_fee,
  name = "pay_mac_fee",
  title = "MAC, Locality, Non-Facility Fee, Facility Fee",
  description = "Annual Physician Fee Schedule Payment Amount File 2024"
)

# Therapy Reduction by MAC and Locality
# A tibble: 8,393 × 5
therapy_reduction <- pfs_pay |>
  dplyr::select(
    hcpcs,
    mac = pmt_mac,
    locality = pmt_locality,
    non_ther_reduct = pmt_therapy_reduction_non,
    fac_ther_reduct = pmt_therapy_reduction_fac
  ) |>
  dplyr::filter(non_ther_reduct > 0 | fac_ther_reduct > 0)

pin_update(
  therapy_reduction,
  name = "therapy_reduction",
  title = "Therapy Reduction by MAC and Locality",
  description = "Annual Physician Fee Schedule Payment Amount File 2024"
)

# OPPS Indicator, Non-Facility Fee, Facility Fee
# A tibble: 104,204 × 5
opps_pay <- pfs_pay |>
  dplyr::select(
    hcpcs,
    mac = pmt_mac,
    locality = pmt_locality,
    opps_ind = pmt_opps_ind,
    opps_non_price = pmt_opps_non,
    opps_fac_price = pmt_opps_fac
  ) |>
  dplyr::filter(opps_ind != 9) |>
  dplyr::select(-opps_ind)

pin_update(
  opps_pay,
  name = "opps_pay",
  title = "OPPS Indicator, Non-Facility Fee, Facility Fee",
  description = "Annual Physician Fee Schedule Payment Amount File 2024"
)
