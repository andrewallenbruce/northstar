library(provider)
library(tidyverse)
library(janitor)

rbcs <- betos(tidy = FALSE) |>
  janitor::clean_names() |>
  dplyr::tibble() |>
  dplyr::mutate(hcpcs_cd_add_dt = convert_to_date(hcpcs_cd_add_dt, character_fun = lubridate::mdy),
                hcpcs_cd_end_dt = convert_to_date(hcpcs_cd_end_dt, character_fun = lubridate::mdy),
                rbcs_assignment_eff_dt = convert_to_date(rbcs_assignment_eff_dt, character_fun = lubridate::mdy),
                rbcs_major_ind = dplyr::case_match(rbcs_major_ind,
                                                   "M" ~ "Major",
                                                   "N" ~ "Non-Procedure",
                                                   "O" ~ "Other")) |>
  dplyr::select(
    hcpcs = hcpcs_cd,
    rbcs = rbcs_id,
    cat.id = rbcs_cat,
    sub.id = rbcs_cat_subcat,
    fam.id = rbcs_fam_numb,
    category = rbcs_cat_desc,
    subcategory = rbcs_subcat_desc,
    family = rbcs_family_desc,
    major = rbcs_major_ind,
    date_hcpcs_add = hcpcs_cd_add_dt,
    date_hcpcs_end = hcpcs_cd_end_dt,
    date_rbcs_assign = rbcs_assignment_eff_dt,
  )

# rbcs |>
#   rowwise() |>
#   mutate(cat1 = is_category_I(hcpcs),
#          cat2 = is_category_II(hcpcs),
#          cat3 = is_category_III(hcpcs),
#          cat4 = is_level_II(hcpcs)) |>
#   count(major)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(rbcs,
                  name = "rbcs",
                  title = "Restructured BETOS Classification for HCPCS",
                  type = "qs")

board |> pins::write_board_manifest()
