source(here::here("data-raw", "pins_functions.R"))

library(googlesheets4)
library(tidyverse)
library(janitor)
library(clock)

# gs_id <- "1MVaKH6T6GZ39iKvrNRoiOBb47RQtMYJ4g0nvunmzsBg"

wkbk <- "1KUPLYD2dksyD4Gcc8pHL5Chw20Sjcck0HYM9VA_JZJ4"
sh1 <- read_sheet(wkbk, sheet = 1, col_types = "iicccccicccc")
sh2 <- read_sheet(wkbk, sheet = 2, col_types = "iicccccicccc")
sh3 <- read_sheet(wkbk, sheet = 3, col_types = "iicccccicccc")

practicum <- vctrs::vec_rbind(
  sh1,
  sh2,
  sh3
) |>
  mutate(
    specialty = case_match(
      specialty,
      "ER VISIT" ~ "Emergency Medicine",
      "CARDIO" ~ "Cardiology",
      "GASTRO" ~ "Gastroenterology",
      "ORTHO" ~ "Orthopedics",
      "RADIO" ~ "Radiology",
      "DERMA" ~ "Dermatology",
      "OBGYN" ~ "Obstetrics/Gynecology",
      "RESPIR" ~ "Respiratory",
      "PEDS" ~ "Pediatrics",
      "PLS SURG" ~ "Plastic Surgery",
      c("NERVOUS", "NEURO") ~ "Neurology",
      "ENT" ~ "Ear, Nose, Throat",
      "INPATIENT" ~ "Inpatient",
      "PSYCH" ~ "Behavioral Health",
      c("GENITOU", "GENITO") ~ "Genitourinary",
      "OSTEO" ~ "Osteopathy",
      "ENDOCR" ~ "Endocrinology",
      c("OPTIC", "OPTHAM") ~ "Opthamology",
      "WORKCMP" ~ "Worker's Comp"
    ) |>
      as_factor(),
    diagnosis = na_if(diagnosis, "-"),
    icd = na_if(icd, "-"),
    procedure = na_if(procedure, "-"),
    cpt = na_if(cpt, "-"),
    mod1 = na_if(mod1, "-"),
    mod2 = na_if(mod2, "-"),
    mod3 = na_if(mod3, "-"),
    notes = na_if(notes, "-"),
  ) |>
  rename(
    icd_10 = icd,
    hcpcs = cpt
  ) |>
  arrange(group, case) |>
  mutate(
    id = consecutive_id(case),
    group = NULL,
    case = NULL,
    .before = 1
  )

# Update pin -------------------
pin_update(
  practicum,
  name = "practicum",
  title = "AAPC CPC Practicum Case Data",
  description = "AAPC CPC Practicum Case Data"
)


# Example usage ----------------
hcpcs_vec <- practicum |>
  filter(!is.na(hcpcs)) |>
  count(hcpcs, sort = TRUE) |>
  pull(hcpcs)

hcpcs_desc <- northstar::get_descriptions(
  hcpcs = hcpcs_vec,
  desc_type = "Long"
) |>
  select(
    hcpcs,
    hcpcs_description = description,
    hcpcs_section = section
  )

practicum_ex <- practicum |>
  left_join(
    hcpcs_desc,
    by = join_by(hcpcs),
    relationship = "many-to-many"
  )

icd_vec <- practicum |>
  filter(!is.na(icd_10)) |>
  count(icd_10, sort = TRUE) |>
  pull(icd_10)

library(pathologie)
library(northstar)

icd_desc <- pathologie::icd10cm(icd = icd_vec) |>
  select(
    icd_10 = icd_code,
    icd_description,
    icd_section = icd_sec_name
    )

practicum_ex <- practicum_ex |>
  left_join(
    icd_desc,
    by = join_by(icd_10),
    relationship = "many-to-many"
  )

mod_vec <- practicum |>
  select(id, mod1, mod2, mod3) |>
  pivot_longer(
    !id,
    names_to = "type",
    values_to = "mod",
    values_drop_na = TRUE
  ) |>
  select(-type) |>
  count(mod, sort = TRUE) |>
  pull(mod)

mod_desc <- search_modifiers(mod = mod_vec) |>
  select(
    mod,
    mod_desc = label
  )

practicum_ex <- practicum_ex |>
  left_join(
    mod_desc,
    by = join_by(mod1 == mod),
    relationship = "many-to-many"
  ) |>
  rename(mod1_desc = mod_desc)

practicum_ex <- practicum_ex |>
  left_join(
    mod_desc,
    by = join_by(mod2 == mod),
    relationship = "many-to-many"
  ) |>
  rename(mod2_desc = mod_desc)

practicum_ex <- practicum_ex |>
  left_join(
    mod_desc,
    by = join_by(mod3 == mod),
    relationship = "many-to-many"
  ) |>
  rename(mod3_desc = mod_desc)

practicum_ex <- practicum_ex |>
  select(
    case_id               = id,
    provider_specialty    = specialty,
    diagnosis_description = diagnosis,
    icd_code              = icd_10,
    icd_description,
    icd_section,
    procedure_description = procedure,
    hcpcs_code            = hcpcs,
    hcpcs_description,
    hcpcs_section,
    hcpcs_units           = units,
    hcpcs_mod1            = mod1,
    hcpcs_mod1_desc       = mod1_desc,
    hcpcs_mod2            = mod2,
    hcpcs_mod2_desc       = mod2_desc,
    hcpcs_mod3            = mod3,
    hcpcs_mod3_desc       = mod3_desc,
    case_notes            = notes
  )

practicum_ex
