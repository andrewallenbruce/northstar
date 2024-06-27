source(here::here("data-raw", "source_setup", "setup.R"))

rbcs <- provider::betos(tidy = FALSE)

rbcs <- rbcs |>
  janitor::clean_names() |>
  dplyr::tibble() |>
  dplyr::reframe(
    hcpcs_code = hcpcs_cd,
    # rbcs_id,
    # rbcs_cat_id = rbcs_cat,
    # rbcs_sub_id = rbcs_cat_subcat,
    # rbcs_fam_id = rbcs_fam_numb,
    rbcs_procedure = rbcs_major_ind,
    rbcs_category = rbcs_cat_desc,
    rbcs_subcategory = rbcs_subcat_desc,
    rbcs_family = rbcs_family_desc,
    hcpcs_date_added = janitor::convert_to_date(hcpcs_cd_add_dt, character_fun = lubridate::mdy),
    hcpcs_date_ended = janitor::convert_to_date(hcpcs_cd_end_dt, character_fun = lubridate::mdy),
    rbcs_date_effective = janitor::convert_to_date(rbcs_assignment_eff_dt, character_fun = lubridate::mdy),
    rbcs_procedure = dplyr::case_match(
      rbcs_major_ind,
      "M" ~ "Major",
      "N" ~ "Non-Procedure",
      "O" ~ "Other"),
    rbcs_category = forcats::as_factor(rbcs_category),
    rbcs_procedure = forcats::as_factor(rbcs_procedure)
    )

hcpcs_gt1 <- rbcs |>
  dplyr::count(
    hcpcs_code,
    rbcs_subcategory,
    rbcs_family,
    sort = TRUE
    ) |>
  dplyr::filter(n > 1) |>
  dplyr::pull(hcpcs_code) |>
  unique()

rbcs |>
  dplyr::filter(rbcs_subcategory == "Breast")
  dplyr::filter(
    hcpcs_code %in% hcpcs_gt1,
    rbcs_family == "No RBCS Family") |>
  dplyr::count(rbcs_subcategory, rbcs_family)

# Update Pin
pin_update(
  rbcs,
  name = "rbcs",
  title = "Restructured BETOS Classification for HCPCS",
  description = "Restructured BETOS Classification for HCPCS"
)

