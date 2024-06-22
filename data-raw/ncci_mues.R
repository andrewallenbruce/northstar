source(here::here("data-raw", "source_setup", "setup.R"))

url_to_scrape <- "https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits"

mue_urls <- read_html(url_to_scrape) |>
  html_elements("a") |>
  html_attr("href") |>
  unique() |>
  str_subset("/files/zip/")

latest_zip_urls <- glue("https://www.cms.gov{mue_urls}")

curl::multi_download(latest_zip_urls)

zip_paths <- fs::file_info(
  fs::dir_ls(path = here::here(),
             glob = "*.zip")) |>
  dplyr::pull(path) |>
  as.character()

# Unzip
purrr::walk(zip_paths, zip::unzip)

# Delete zips
fs::file_delete(here::here(fs::dir_ls(glob = "*.zip")))

xlsx_paths <- fs::file_info(fs::dir_ls(here::here())) |>
  dplyr::pull(path) |>
  as.character() |>
  str_subset("xlsx") |>
  str_subset("Changes", negate = TRUE)

# Delete txts and csvs
fs::file_delete(here::here(fs::dir_ls(glob = "*.csv|*.txt")))

mues <- xlsx_paths |>
  map(read_excel, col_types = "text") |>
  map(fuimus::df_2_chr) |>
  set_names(str_remove_all(basename(xlsx_paths), ".xlsx"))

mue_pract <- mues$`MCR_MUE_PractitionerServices_Eff_07-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  reframe(
    hcpcs_code = hcpcs_cpt_code,
    mue_service = "Practitioner",
    mue_units = as.integer(practitioner_services_mue_values),
    mue_mai = as.integer(substr(mue_adjudication_indicator, 1, 1)),
    mue_mai_desc = substr(mue_adjudication_indicator, 3, 100),
    mue_rationale)

mue_oph <- mues$`MCR_MUE_OutpatientHospitalServices_Eff_07-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  reframe(
    hcpcs_code = hcpcs_cpt_code,
    mue_service = "Outpatient Hospital",
    mue_units = as.integer(outpatient_hospital_services_mue_values),
    mue_mai = as.integer(substr(mue_adjudication_indicator, 1, 1)),
    mue_mai_desc = substr(mue_adjudication_indicator, 3, 100),
    mue_rationale)

mue_dme <- mues$`MCR_MUE_DMESupplierServices_Eff_07-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  reframe(
    hcpcs_code = hcpcs_cpt_code,
    mue_service = "DME Supplier",
    mue_units = as.integer(dme_supplier_services_mue_values),
    mue_mai = as.integer(substr(mue_adjudication_indicator, 1, 1)),
    mue_mai_desc = substr(mue_adjudication_indicator, 3, 100),
    mue_rationale)

mue_base <- vctrs::vec_rbind(mue_pract, mue_oph, mue_dme) |>
  dplyr::mutate(
    mue_mai_desc = case_match(
      mue_mai_desc,
      "Date of Service Edit: Clinical" ~ "DOS: Clinical",
      "Date of Service Edit: Policy" ~ "DOS: Policy",
      "Line Edit" ~ "Line Edit",
      .default = mue_mai_desc
    )
  )

library(zeallot)

mue_base |>
  select(-mue_mai_desc) |>
  relocate(mue_service, .after = mue_rationale) |>
  hacksaw::eval_split(
  filter(mue_service == "Practitioner"),
  filter(mue_service == "Outpatient Hospital"),
  filter(mue_service == "DME Supplier")
  ) %->% c(mue_prac, mue_out, mue_dme)

mue_prac |>
  distinct()

fs::file_delete(here::here(fs::dir_ls(glob = "*.xlsx")))

# Update Pin
pin_update(
  mue_prac,
  name = "ncci_mue_prac",
  title = "MUEs Practitioner",
  description = "NCCI Medically Unlikely Edits 07-01-2024: Practitioner Services"
)

pin_update(
  mue_out,
  name = "ncci_mue_out",
  title = "MUEs Outpatient Hospital",
  description = "NCCI Medically Unlikely Edits 07-01-2024: Outpatient Hospital Services"
)

pin_update(
  mue_dme,
  name = "ncci_mue_dme",
  title = "MUEs DME Supplier",
  description = "NCCI Medically Unlikely Edits 07-01-2024: DME Supplier Services"
)

# 17,726
base <- mue_base|>
  dplyr::select(-mue_service, -mue_units) |>
  dplyr::distinct()

# 18,135
# mue_base|>
#   dplyr::select(-mue_service) |>
#   dplyr::distinct()

# 14,677
prac <- mue_base |>
  filter(mue_service == "Practitioner") |>
  select(-mue_service) |>
  rename(prac = mue_units)

# 14,628
out <- mue_base |>
  filter(mue_service == "Outpatient Hospital") |>
  select(-mue_service) |>
  rename(out = mue_units)

# 2,928
dme <- mue_base |>
  filter(mue_service == "DME Supplier") |>
  select(-mue_service) |>
  rename(dme = mue_units)

init_join <- base |>
  left_join(prac) |>
  left_join(out) |>
  left_join(dme)

eq_test <- init_join |>
  rowwise() |>
  mutate(all = n_distinct(c(prac, out, dme)) == 1,
         prac_out = n_distinct(c(prac, out)) == 1,
         prac_dme = n_distinct(c(prac, dme)) == 1,
         out_dme = n_distinct(c(out, dme)) == 1) |>
  ungroup() |>
  mutate(
    mue_services = case_when(
      all ~ "All",
      prac_out ~ "Practitioner & Outpatient Hospital",
      prac_dme ~ "Practitioner & DME Supplier",
      out_dme ~ "Outpatient Hospital & DME Supplier",
      .default = NA_character_),
    mue_units = case_when(
      all ~ prac,
      prac_out ~ prac,
      prac_dme ~ prac,
      out_dme ~ out,
      .default = NA_integer_)
    )


eq_1 <- eq_test |>
  filter(!is.na(mue_services)) |>
  select(-c(prac, out, dme, all, prac_out, prac_dme, out_dme))

eq_2 <- eq_test |>
  filter(is.na(mue_services)) |>
  select(-c(all, prac_out, prac_dme, out_dme, mue_services, mue_units))

eq_prac <- eq_2 |>
  select(-out, -dme) |>
  filter(!is.na(prac)) |>
  rename(mue_units = prac) |>
  mutate(mue_services = "Practitioner")

eq_out <- eq_2 |>
  select(-prac, -dme) |>
  filter(!is.na(out)) |>
  rename(mue_units = out) |>
  mutate(mue_services = "Outpatient Hospital")

eq_dme <- eq_2 |>
  select(-prac, -out) |>
  filter(!is.na(dme)) |>
  rename(mue_units = dme) |>
  mutate(mue_services = "DME Supplier")

vctrs::vec_rbind(
  eq_1,
  eq_prac,
  eq_out,
  eq_dme
) |>
  select(
    hcpcs_code,
    mue_units,
    mue_mai,
    # mue_mai_desc,
    mue_rationale,
    mue_services
  )


# mue_mai mue_mai_desc                       n
#       3 Date of Service Edit: Clinical 11237
#       2 Date of Service Edit: Policy    6424
#       1 Line Edit                         65

#    mue_rationale                         n
#    <chr>                             <int>
#  1 CMS Policy                         5129
#  2 Code Descriptor / CPT Instruction  3055
#  3 Clinical: Data                     3015
#  4 Anatomic Consideration             2896
#  5 Nature of Service/Procedure        1190
#  6 Prescribing Information             656
#  7 Clinical: CMS Workgroup             586
#  8 Nature of Analyte                   561
#  9 Nature of Equipment                 255
# 10 Published Contractor Policy         195
# 11 Drug discontinued                    73
# 12 Clinical: Society Comment            43
# 13 Oral Medication; Not Payable         39
# 14 Compounded Drug Policy               33
