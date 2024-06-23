source(here::here("data-raw", "source_setup", "setup.R"))

#----- NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE zip file
url_to_scrape <- "https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files"

url_to_click <- read_html(url_to_scrape)

url_to_click |>
  html_elements("a") |>
  html_attr("href") |>
  unique() |>
  str_subset(glue("{url_to_scrape}/rvu{substr(Sys.Date(), 3, 4)}"))

pg <- session(url_to_scrape) |>
  session_jump_to(url_to_click[1]) |>
  session_follow_link("RVU24C")

latest_zip_url <- pg$url |> url_absolute("https://www.cms.gov")

"https://www.cms.gov/files/zip/rvu24c.zip" == latest_zip_url

curl::multi_download("https://www.cms.gov/files/zip/rvu24c.zip")

xlsx_filename <- zip::zip_list(
  fs::dir_ls(glob = "*.zip")) |>
  filter(str_detect(filename, ".xlsx")) |>
  pull(filename)

zip::unzip(fs::dir_ls(glob = "*.zip"), files = xlsx_filename)

rvu_files <- here::here(xlsx_filename) |>
  purrr::map(readxl::read_excel, col_types = "text") |>
  purrr::map(fuimus::df_2_chr) |>
  purrr::set_names(stringr::str_remove_all(xlsx_filename, ".xlsx")) |>
  purrr::map(janitor::clean_names)

fs::file_delete(here::here(fs::dir_ls(glob = "*.zip")))

#----- RVU File July 2024
rvu_names <- c(
  'hcpcs',
  'mod',
  'description',
  'status_code',
  'not_used_for_medicare_payment',
  'work_rvu',
  'non_fac_pe_rvu',
  'non_fac_na_indicator',
  'facility_pe_rvu',
  'facility_na_indicator',
  'mp_rvu',
  'non_facility_total',
  'facility_total',
  'pctc_ind',
  'glob_days',
  'pre_op',
  'intra_op',
  'post_op',
  'mult_proc',
  'bilat_surg',
  'asst_surg',
  'co_surg',
  'team_surg',
  'endo_base',
  'conv_factor',
  'physician_supervision_of_diagnostic_procedures',
  'calculation_flag',
  'diagnostic_imaging_family_indicator',
  'non_facility_pe_used_for_opps_payment_amount',
  'facility_pe_used_for_opps_payment_amount',
  'mp_used_for_opps_payment_amount'
)

rvu24_jul <- rvu_files$PPRRVU24_JUL |>
  janitor::row_to_names(row_number = "find_header") |>
  janitor::clean_names() |>
  rlang::set_names(rvu_names) |>
  dplyr::mutate(
    dplyr::across(
      c(
      work_rvu,
      non_fac_pe_rvu,
      facility_pe_rvu,
      non_facility_total,
      facility_total,
      mp_rvu,
      conv_factor,
      pre_op,
      intra_op,
      post_op,
      non_facility_pe_used_for_opps_payment_amount,
      facility_pe_used_for_opps_payment_amount,
      mp_used_for_opps_payment_amount
    ),
    readr::parse_number
  )) |>
  dplyr::rename(
    hcpcs_code             = hcpcs,
    hcpcs_description      = description,
    rvu_status_code        = status_code,
    rvu_work               = work_rvu,
    rvu_mp                 = mp_rvu,
    rvu_non_pe             = non_fac_pe_rvu,
    rvu_fac_pe             = facility_pe_rvu,
    rvu_conv_factor        = conv_factor,
    rvu_non_total          = non_facility_total,
    rvu_fac_total          = facility_total,
    rvu_opps_non_pe        = non_facility_pe_used_for_opps_payment_amount,
    rvu_opps_fac_pe        = facility_pe_used_for_opps_payment_amount,
    rvu_opps_mp            = mp_used_for_opps_payment_amount,
    rvu_glob_days          = glob_days,
    rvu_pre_op             = pre_op,
    rvu_intra_op           = intra_op,
    rvu_post_op            = post_op,
    rvu_bilat_surg         = bilat_surg,
    rvu_asst_surg          = asst_surg,
    rvu_co_surg            = co_surg,
    rvu_team_surg          = team_surg,
    rvu_mult_proc          = mult_proc,
    rvu_mod                = mod,
    rvu_pctc_ind           = pctc_ind,
    rvu_endo_base          = endo_base,
    rvu_phys_sup_diag_proc = physician_supervision_of_diagnostic_procedures,
    rvu_diag_img_fam_ind   = diagnostic_imaging_family_indicator,
    rvu_non_na_ind         = non_fac_na_indicator,
    rvu_fac_na_ind         = facility_na_indicator,
    rvu_not_used_mcr_pmt   = not_used_for_medicare_payment,
    rvu_calc_flag          = calculation_flag) |>
  dplyr:::mutate(
    rvu_op_ind = rvu_pre_op + rvu_intra_op + rvu_post_op,
    .before = rvu_pre_op)

pin_update(
  rvu24_jul,
  name = "pfs_rvu",
  title = "RVU File July 2024",
  description = "National Physician Fee Schedule Relative Value File July 2024"
)

#----- OPPSCAP File July 2024
# OPPSCAP contains the payment amounts after the application of the
# OPPS-based payment caps, except for carrier priced codes. For carrier
# price codes, the field only contains the OPPS-based payment caps. Carrier
# prices cannot exceed the OPPS-based payment caps.

opps <- rvu_files$OPPSCAP_JUL |>
  dplyr::mutate(
    dplyr::across(
      c(non_facilty_price, facility_price),
      readr::parse_number)) |>
  dplyr::filter(hcpcs != "\u001a") |>
  dplyr::select(
    hcpcs_code = hcpcs,
    opps_mod = mod,
    opps_procstat = procstat,
    opps_carrier = carrier,
    opps_locality = locality,
    opps_fac_price = facility_price,
    opps_non_price = non_facilty_price
  )

# Update Pin
pin_update(
  opps,
  name = "pfs_opps",
  title = "PFS OPPSCAP July 2024",
  description = "OPPSCAP contains the payment amounts after the application of the OPPS-based payment caps, except for carrier priced codes. For carrier price codes, the field only contains the OPPS-based payment caps. Carrier prices cannot exceed the OPPS-based payment caps."
)

delete_pins("opps")
list_pins()

# ADDENDUM E. FINAL CY 2024 GEOGRAPHIC PRACTICE COST INDICES (GPCIs) BY STATE AND MEDICARE LOCALITY
# https://www.ama-assn.org/system/files/geographic-practice-cost-indices-gpcis.pdf

gpci <- rvu_files$GPCI2024 |>
  janitor::row_to_names(row_number = "find_header") |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(state)) |>
  dplyr::reframe(
    mac = medicare_administrative_contractor_mac,
    state = state,
    locality_number,
    gpci_work = x2024_pw_gpci_with_1_0_floor,
    gpci_pe = x2024_pe_gpci,
    gpci_mp = x2024_mp_gpci,
    locality_name) |>
  dplyr::mutate(
    dplyr::across(
      c(gpci_work, gpci_pe, gpci_mp), readr::parse_number),
    locality_name = stringr::str_remove_all(locality_name, stringr::fixed("*"))
         )

# Update Pin
pin_update(
  gpci,
  name = "pfs_gpci",
  title = "GPCI July 2024",
  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024"
)

# counties_included_in_2024_localities_alphabetically_by_state_and_locality_name_within_state
# * = Payment locality is serviced by two carriers.

locco <- rvu_files$`24LOCCO` |>
  janitor::row_to_names(row_number = "find_header") |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(medicare_adminstrative_contractor)) |>
  tidyr::fill(state) |>
  dplyr::reframe(
    mac = medicare_adminstrative_contractor,
    locality = locality_number,
    state_name = state,
    fee_schedule_area = stringr::str_remove_all(
      fee_schedule_area, stringr::fixed("*")),
    counties
    )

# Update Pin
pin_update(
  locco,
  name = "pfs_locco",
  title = "GPCI Counties included in Localities July 2024",
  description = "GPCI Counties included in Localities July 2024"
)

df_state <- dplyr::tibble(
  state_abb = state.abb,
  state_name = toupper(state.name)
)

gpci |>
  left_join(df_state) |>
  mutate(state_name = case_match(state_abb,
      "DC" ~ "DISTRICT OF COLUMBIA",
      "PR" ~ "PUERTO RICO",
      "VI" ~ "VIRGIN ISLANDS",
      .default = state_name)) |>
  full_join(locco)

locco


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

anesthesia <- rvu_files$ANES2024 |>
  janitor::clean_names() |>
  dplyr::mutate(
    x2024_anesthesia_conversion_factor = as.double(x2024_anesthesia_conversion_factor)) |>
  dplyr::rename(anesthesia_cf = x2024_anesthesia_conversion_factor)

# Update Pin
pin_update(
  anesthesia,
  name = "pfs_anesthesia",
  title = "Anesthesia Conversion Factor July 2024",
  description = "Anesthesia Conversion Factor July 2024"
)


fs::file_delete(here::here(fs::dir_ls(glob = "*.zip|*.xlsx")))
