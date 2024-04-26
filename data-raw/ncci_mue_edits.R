source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

mues <- mue_paths |>
  map(read_excel, col_types = "text") |>
  map(fuimus::df_2_chr)

# Medicare NCCI Medically Unlikely Edits (MUEs)
# https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits
mue_pract <- mues$`MCR_MUE_PractitionerServices_Eff_04-01-2024` |>
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

mue_outhosp <- mues$`MCR_MUE_OutpatientHospitalServices_Eff_04-01-2024` |>
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

mue_dme <- mues$`MCR_MUE_DMESupplierServices_Eff_04-01-2024` |>
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

mue <- vctrs::vec_rbind(mue_pract, mue_outhosp, mue_dme) |>
  dplyr::select(
    hcpcs,
    mue_uos = mue,
    mue_mai = mai,
    mue_mai_desc = adjudication,
    mue_service_type = service_type,
    mue_rationale = rationale
  )

# mue_mai                                        n
#   <int>                                    <int>
#      3 (Date of Service Edit: Clinical)    19983
#      2 (Date of Service Edit: Policy)      12113
#      1 (Line Edit)                           106

# mue_rationale                         n
# <chr>                             <int>
# CMS Policy                         8546
# Clinical: Data                     5684
# Anatomic Consideration             5679
# Code Descriptor / CPT Instruction  5569
# Nature of Service/Procedure        2332
# Prescribing Information            1263
# Nature of Analyte                  1116
# Clinical: CMS Workgroup             959
# Nature of Equipment                 417
# Drug discontinued                   212
# Published Contractor Policy         204
# Compounded Drug Policy               99
# Clinical: Society Comment            78
# Oral Medication; Not Payable         44

# Update Pin
pin_update(
  mue,
  name = "mues",
  title = "Medically Unlikely Edits",
  description = "Medicare NCCI Medically Unlikely Edits (MUEs)"
)
