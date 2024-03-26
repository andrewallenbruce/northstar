library(readxl)
library(tidyverse)
library(janitor)

# NCCI Files Updated Quarterly
paths <- fs::dir_ls("C:/Users/Andrew/Desktop/payer_guidelines/data/NCCI/", regexp = "*.xlsx$")
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

# Medicare NCCI Add-on Code Edits 2024-04-01
# https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits
aoc <- ncci$`AOC_V2024Q2-MCR` |>
  clean_names() |>
  mutate(
    aoc_del_dt                = as.integer(substr(aoc_del_dt, 1, 4)),
    primary_code_del_dt       = as.integer(substr(primary_code_del_dt, 1, 4)),
    aoc_edit_eff_dt           = as.integer(substr(aoc_edit_eff_dt, 1, 4)),
    aoc_edit_eff_dt           = as.integer(substr(aoc_edit_eff_dt, 1, 4)),
    aoc_edit_type             = as.integer(aoc_edit_type),
    special_instruction_notes = str_remove_all(special_instruction_notes, regex('\\(|\\)|\\"'))) |>
  select(
    addon    = add_on_code,
    primary  = primary_code,
    edit     = aoc_edit_type,
    add_del  = aoc_del_dt,
    prim_del = primary_code_del_dt,
    edit_eff = aoc_edit_eff_dt,
    edit_del = aoc_edit_del_dt,
    notes    = special_instruction_notes
  )

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(
    aoc,
    name = "aoc",
    title = "Add-on Code Edits",
    description = "Medicare NCCI Add-on Code Edits 2024-04-01",
    type = "qs"
  )

board |> pins::write_board_manifest()

# Medicare NCCI Medically Unlikely Edits (MUEs)
# https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-medically-unlikely-edits
mue_pract <- ncci$`MCR_MUE_PractitionerServices_Eff_04-01-2024` |>
  clean_names() |>
  mutate(practitioner_services_mue_values = as.integer(practitioner_services_mue_values),
         adj                              = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication                     = substr(mue_adjudication_indicator, 3, 100),
         type                             = "Practitioner") |>
  select(hcpcs                            = hcpcs_cpt_code,
         mues                             = practitioner_services_mue_values,
         adj,
         adjudication,
         rationale                        = mue_rationale,
         type
    )

mue_outhosp <- ncci$`MCR_MUE_OutpatientHospitalServices_Eff_04-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  mutate(outpatient_hospital_services_mue_values = as.integer(outpatient_hospital_services_mue_values),
         adj                                     = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication                            = substr(mue_adjudication_indicator, 3, 100),
         type                                    = "Outpatient Hospital") |>
  select(hcpcs                                   = hcpcs_cpt_code,
         mues                                    = outpatient_hospital_services_mue_values,
         adj,
         adjudication,
         rationale                               = mue_rationale,
         type
  )

mue_dme <- ncci$`MCR_MUE_DMESupplierServices_Eff_04-01-2024` |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  mutate(dme_supplier_services_mue_values = as.integer(dme_supplier_services_mue_values),
         adj = as.integer(substr(mue_adjudication_indicator, 1, 1)),
         adjudication = substr(mue_adjudication_indicator, 3, 100),
         type = "DME Supplier") |>
  select(hcpcs = hcpcs_cpt_code,
         mues = dme_supplier_services_mue_values,
         adj,
         adjudication,
         rationale = mue_rationale,
         type
  )

mue <- vctrs::vec_rbind(mue_pract, mue_outhosp, mue_dme)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(
    mue,
    name = "mues",
    title = "Medically Unlikely Edits",
    description = "Medicare NCCI Medically Unlikely Edits (MUEs)",
    type = "qs"
  )

board |> pins::write_board_manifest()

# Medicare NCCI Procedure to Procedure (PTP) Edits
# Column 3: * = in existence prior to 1996
# Column 4: Modifier 0 = Not Allowed, 1 = Allowed, 9 = Not Applicable
# https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits
ptp1 <- ncci$`ccipra-v301r0-f1` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
    )


ptp2 <- ncci$`ccipra-v301r0-f2` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp3 <- ncci$`ccipra-v301r0-f3` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp4 <- ncci$`ccipra-v301r0-f4` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp <- vctrs::vec_rbind(ptp1, ptp2, ptp3, ptp4)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(
    ptp,
    name = "ptp",
    title = "PTP Edits",
    description = "Medicare NCCI Procedure to Procedure (PTP) Edits",
    type = "qs"
  )

board |> pins::write_board_manifest()
