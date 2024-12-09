source(here::here("data-raw", "source_setup", "setup.R"))

#----- NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE zip file
url_to_scrape <- "https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files"

url_to_click <- rvest::read_html(url_to_scrape)

rvu_prefix <- url_to_click |>
  rvest::html_elements("a") |>
  rvest::html_attr("href") |>
  unique() |>
  stringr::str_subset(
    stringr::regex(
      as.character(
        glue::glue(
          "/medicare/payment/fee-schedules/physician/pfs-relative-value-files/rvu<<substr(Sys.Date(), 3, 4)>>[a-z]{1,2}",
          .open = "<<",
          .close = ">>"
          )
        )
      )
    ) |>
  stringr::str_replace_all(
    "/medicare/payment/fee-schedules/physician/pfs-relative-value-files/",
    "https://www.cms.gov/files/zip/"
    ) |>
  stringr::str_c(".zip")


# pg <- session(url_to_scrape) |>
#   session_jump_to(url_to_click[1]) |>
#   session_follow_link("RVU24C")
# latest_zip_url <- pg$url |> url_absolute("https://www.cms.gov")
# "https://www.cms.gov/files/zip/rvu24c.zip" == latest_zip_url

curl::multi_download(rvu_prefix)

xlsx_filename <- zip::zip_list(
  fs::dir_ls(glob = "*c.zip")) |>
  filter(str_detect(filename, ".xlsx")) |>
  pull(filename)

zip::unzip(fs::dir_ls(glob = "*c.zip"),
           files = xlsx_filename)

rvu_files <- here::here(xlsx_filename) |>
  purrr::map(readxl::read_excel, col_types = "text") |>
  purrr::map(fuimus::df_2_chr) |>
  purrr::set_names(stringr::str_remove_all(xlsx_filename, ".xlsx")) |>
  purrr::map(janitor::clean_names)

fs::file_delete(here::here(fs::dir_ls(glob = "*.zip")))

#----- RVU File July 2024
rvu_names <- c(
  'hcpcs_code',
  'mod',
  'hcpcs_description',
  'status',
  'not_used_for_mcr_pmt',
  'rvu_work',
  'rvu_non_pe',
  'non_na_ind',
  'rvu_fac_pe',
  'fac_na_ind',
  'rvu_mp',
  'rvu_non_total',
  'rvu_fac_total',
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
  'cf',
  'phys_sup_diag_proc',
  'calc_flag',
  'diag_img_fam_ind',
  'rvu_opps_non_pe',
  'rvu_opps_fac_pe',
  'rvu_opps_mp'
)

rvu24_jul <- rvu_files$PPRRVU24_JUL |>
  janitor::row_to_names(row_number = "find_header") |>
  janitor::clean_names() |>
  rlang::set_names(rvu_names) |>
  dplyr::filter(!is.na(calc_flag)) |>
  dplyr::mutate(
    dplyr::across(
      c(dplyr::contains("rvu"),
        dplyr::contains("surg"),
        cf,
        pre_op,
        intra_op,
        post_op,
        pctc_ind,
        mult_proc
      ),
      readr::parse_number),
    op_ind = as.integer(pre_op + intra_op + post_op)
  ) |>
  dplyr::select(
    hcpcs_code,
    mod,
    status,
    rvu_work,
    rvu_non_pe,
    rvu_fac_pe,
    rvu_mp,
    rvu_non_sum = rvu_non_total,
    rvu_fac_sum = rvu_fac_total,
    rvu_opps_non_pe,
    rvu_opps_fac_pe,
    rvu_opps_mp,
    cf,
    pctc = pctc_ind,
    glob = glob_days,
    # op_ind,
    pre_op,
    intra_op,
    post_op,
    mult_proc,
    bilat_surg,
    asst_surg,
    co_surg,
    team_surg,
    endo_base,
    phys_diag_pro = phys_sup_diag_proc,
    diag_img_fam = diag_img_fam_ind,
    non_na = non_na_ind,
    fac_na = fac_na_ind,
    not_used_mcr = not_used_for_mcr_pmt
  )

# rvu24_jul |>
#   hacksaw::count_split(
#     not_used_for_mcr_pmt,
#     non_na_ind,
#     fac_na_ind,
#     pctc_ind,
#     mult_proc,
#     bilat_surg,
#     asst_surg,
#     co_surg,
#     team_surg
#     )

rvu_desc <- rvu24_jul |>
  dplyr::reframe(
    hcpcs_code,
    hcpcs_desc_type = "Short",
    hcpcs_description,
  )

pin_update(
  rvu_desc,
  name = "rvu_descriptions",
  title = "RVU HCPCS Descriptions July 2024",
  description = "RVU HCPCS Descriptions July 2024"
)


pfs_rvu_amt <- rvu24_jul |>
  dplyr::select(
    hcpcs_code,
    mod,
    rvu_work,
    rvu_non_pe,
    rvu_fac_pe,
    rvu_mp,
    rvu_non_sum,
    rvu_fac_sum,
    rvu_opps_non_pe,
    rvu_opps_fac_pe,
    rvu_opps_mp,
    cf
  )

pfs_rvu_ind <- rvu24_jul |>
  dplyr::select(
    hcpcs_code,
    mod,
    status,
    pctc,
    glob,
    pre_op,
    intra_op,
    post_op,
    mult_proc,
    bilat_surg,
    asst_surg,
    co_surg,
    team_surg,
    endo_base,
    phys_diag_pro,
    diag_img_fam,
    non_na,
    fac_na,
    not_used_mcr
  )

pfs_rvu_ind <- dplyr::bind_cols(
  get_pin("pfs_rvu_ind"),
  get_pin("pfs_rvu_amt") |> dplyr::select(mod)
) |>
  dplyr::select(
    hcpcs_code,
    mod,
    dplyr::everything()
  )

pin_update(
  pfs_rvu_amt,
  name = "pfs_rvu_amt",
  title = "RVU File Amounts July 2024",
  description = "National Physician Fee Schedule Relative Value File July 2024"
)

pin_update(
  pfs_rvu_ind,
  name = "pfs_rvu_ind",
  title = "RVU File Indicators July 2024",
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
    mod = mod,
    status = procstat,
    mac = carrier,
    locality = locality,
    opps_fac_price = facility_price,
    opps_non_price = non_facilty_price
  )

opps |>
  hacksaw::count_split(
    hcpcs_code,
    mod,
    status,
    mac,
    locality
    )


# Update Pin
pin_update(
  opps,
  name = "pfs_opps",
  title = "PFS OPPSCAP July 2024",
  description = "OPPSCAP contains the payment amounts after the application of the OPPS-based payment caps, except for carrier priced codes. For carrier price codes, the field only contains the OPPS-based payment caps. Carrier prices cannot exceed the OPPS-based payment caps."
)

# ADDENDUM E. FINAL CY 2024 GEOGRAPHIC PRACTICE COST INDICES (GPCIs) BY STATE AND MEDICARE LOCALITY
# https://www.ama-assn.org/system/files/geographic-practice-cost-indices-gpcis.pdf

gpci <- rvu_files$GPCI2024 |>
  janitor::row_to_names(row_number = "find_header") |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(state)) |>
  dplyr::reframe(
    mac = medicare_administrative_contractor_mac,
    state = state,
    locality = locality_number,
    gpci_work = x2024_pw_gpci_with_1_0_floor,
    gpci_pe = x2024_pe_gpci,
    gpci_mp = x2024_mp_gpci,
    locality_name) |>
  dplyr::mutate(
    dplyr::across(c(gpci_work, gpci_pe, gpci_mp), readr::parse_number),
    locality_name = stringr::str_remove_all(locality_name, stringr::fixed("*"))
         )

gpci <- get_pin("pfs_gpci") |>
  dplyr::reframe(
    mac,
    state = forcats::as_factor(state),
    locality,
    locality_name,
    gpci_work,
    gpci_pe,
    gpci_mp,
    gpci_gaf = gpci_work + gpci_pe + gpci_mp)

# Update Pin
pin_update(
  gpci,
  name = "pfs_gpci",
  title = "GPCI July 2024",
  description = "Geographic Practice Cost Indices (GPCIs) by State and Medicare Locality 2024"
)

# counties_included_in_2024_localities_alphabetically_
# by_state_and_locality_name_within_state
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
  dplyr::reframe(
    mac = contractor,
    locality,
    # locality_name,
    anes_cf = as.double(x2024_anesthesia_conversion_factor))

# Update Pin
pin_update(
  anesthesia,
  name = "pfs_anes",
  title = "Anesthesia Conversion Factor July 2024",
  description = "Anesthesia Conversion Factor July 2024"
)

fs::file_delete(here::here(fs::dir_ls(glob = "*.zip|*.xlsx")))
