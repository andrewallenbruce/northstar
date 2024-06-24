source(here::here("data-raw", "source_setup", "setup.R"))

# ANNUAL PHYSICIAN FEE SCHEDULE PAYMENT AMOUNT FILE
# https://www.cms.gov/medicare/payment/fee-schedules/physician/national-payment-amount-file
# https://www.cms.gov/medicare/payment/fee-schedules/physician/carrier-specific-files/all-states
# [990,482 x 15]


filenames <- zip::zip_list(
  fs::dir_ls(glob = "*.zip")) |>
  filter(str_detect(filename, ".txt")) |>
  pull(filename)

zip::unzip(here::here("PFREV24A.zip"))
# zip::unzip(fs::dir_ls(glob = "*.zip"), files = filenames)

pay_names <- c(
  "year",
  "mac",
  "locality",
  "hcpcs_code",
  "mod",
  "non_fac_amt",
  "fac_amt",
  "flat_visit_fee",
  "pctc_ind",
  "status",
  "mult_surg",
  "ther_50pct_non",
  "ther_50pct_fac",
  "opps_ind",
  "opps_amt_non",
  "opps_amt_fac"
)

pmt_file <- here::here("PFALL24.txt") |>
  purrr::map(
    readr::read_csv,
    col_types = strrep("c", 16),
    col_names = pay_names) |>
  purrr::list_rbind() |>
  dplyr::filter(!is.na(hcpcs_code)) |>
  dplyr::mutate(
    dplyr::across(
      c(
        dplyr::contains("_amt"),
        dplyr::contains("ther_50"),
        dplyr::contains("_ind"),
        dplyr::contains("opps_ind")
      ),
      readr::parse_number)) |>
  dplyr::select(
    hcpcs_code,
    mac,
    locality,
    status,
    mod,
    pctc_ind,
    mult_surg,
    non_fac_amt,
    fac_amt,
    ther_50pct_non,
    ther_50pct_fac,
    opps_ind,
    opps_amt_non,
    opps_amt_fac
  )

fs::file_delete(here::here(fs::dir_ls(glob = "*.zip|*.txt|*.pdf")))

pmt_file <- pmt_file |>
  dplyr::mutate(mult_surg = as.numeric(mult_surg))

pin_update(
  pmt_file,
  name = "pfs_pmt",
  title = "Physician Fee Schedule Payment Amount File July 2024",
  description = "Physician Fee Schedule Payment Amount File July 2024"
)
