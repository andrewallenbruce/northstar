source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

rbcs <- betos(tidy = FALSE) |>
  janitor::clean_names() |>
  dplyr::tibble() |>
  dplyr::mutate(
    hcpcs_cd_add_dt        = convert_to_date(hcpcs_cd_add_dt, character_fun = lubridate::mdy),
    hcpcs_cd_end_dt        = convert_to_date(hcpcs_cd_end_dt, character_fun = lubridate::mdy),
    rbcs_assignment_eff_dt = convert_to_date(rbcs_assignment_eff_dt, character_fun = lubridate::mdy),
    rbcs_major_ind         = dplyr::case_match(rbcs_major_ind,
    "M" ~ "Major",
    "N" ~ "Non-Procedure",
    "O" ~ "Other")) |>
  dplyr::select(
    hcpcs = hcpcs_cd,
    rbcs_id,
    rbcs_cat_id = rbcs_cat,
    rbcs_sub_id = rbcs_cat_subcat,
    rbcs_fam_id = rbcs_fam_numb,
    rbcs_category = rbcs_cat_desc,
    rbcs_subcategory = rbcs_subcat_desc,
    rbcs_family = rbcs_family_desc,
    rbcs_procedure = rbcs_major_ind,
    rbcs_date_hcpcs_add = hcpcs_cd_add_dt,
    rbcs_date_hcpcs_end = hcpcs_cd_end_dt,
    rbcs_date_rbcs_asn = rbcs_assignment_eff_dt,
  )

# Update Pin
pin_update(
  rbcs,
  name = "rbcs",
  title = "Restructured BETOS Classification for HCPCS"
)
