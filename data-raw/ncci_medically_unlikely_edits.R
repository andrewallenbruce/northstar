library(readxl)
library(tidyverse)
library(janitor)

# NCCI Files Updated Quarterly
paths <- fs::dir_ls("C:/Users/Andrew/Desktop/payer_guidelines/data/NCCI/", regexp = "*2024.xlsx$")
names <- paths |> basename() |> str_remove_all(pattern = fixed(".xlsx"))
names(paths) <- names

df2chr <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), as.character))
}

ncci <- paths |>
  map(read_excel, col_types = "text") |>
  map(df2chr)

# Medicare NCCI Medically Unlikely Edits (MUEs)
# https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits
mue_pract <- ncci$`MCR_MUE_PractitionerServices_Eff_04-01-2024` |>
  clean_names() |>
  mutate(practitioner_services_mue_values = as.integer(practitioner_services_mue_values),
         mai                              = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication                     = substr(mue_adjudication_indicator, 3, 100),
         service_type                     = "Practitioner") |>
  select(hcpcs                            = hcpcs_cpt_code,
         mue                              = practitioner_services_mue_values,
         mai,
         adjudication,
         rationale                        = mue_rationale,
         service_type
  )

mue_outhosp <- ncci$`MCR_MUE_OutpatientHospitalServices_Eff_04-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  mutate(outpatient_hospital_services_mue_values = as.integer(outpatient_hospital_services_mue_values),
         mai                                     = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication                            = substr(mue_adjudication_indicator, 3, 100),
         service_type                            = "Outpatient Hospital") |>
  select(hcpcs                                   = hcpcs_cpt_code,
         mue                                     = outpatient_hospital_services_mue_values,
         mai,
         adjudication,
         rationale                               = mue_rationale,
         service_type
  )

mue_dme <- ncci$`MCR_MUE_DMESupplierServices_Eff_04-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  mutate(dme_supplier_services_mue_values = as.integer(dme_supplier_services_mue_values),
         mai                              = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication                     = substr(mue_adjudication_indicator, 3, 100),
         service_type                     = "DME Supplier") |>
  select(hcpcs                            = hcpcs_cpt_code,
         mue                              = dme_supplier_services_mue_values,
         mai,
         adjudication,
         rationale                        = mue_rationale,
         service_type
  )

mue <- vctrs::vec_rbind(mue_pract, mue_outhosp, mue_dme)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    mue,
    name = "mues",
    title = "Medically Unlikely Edits",
    description = "Medicare NCCI Medically Unlikely Edits (MUEs)",
    type = "qs"
  )

board |> pins::write_board_manifest()
