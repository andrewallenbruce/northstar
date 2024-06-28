source(here::here("data-raw", "source_setup", "setup.R"))


# Report Example ----------------
report <- qs::qread(here::here("data-raw", "rpt_clean"))


# Practicum Example ----------------
wkbk <- "1KUPLYD2dksyD4Gcc8pHL5Chw20Sjcck0HYM9VA_JZJ4"
sh1 <- read_sheet(wkbk, sheet = 1, col_types = "iicccccicccc")
sh2 <- read_sheet(wkbk, sheet = 2, col_types = "iicccccicccc")
sh3 <- read_sheet(wkbk, sheet = 3, col_types = "iicccccicccc")

# What year were these cases from?
practicum_raw <- vctrs::vec_rbind(
  sh1,
  sh2,
  sh3
)

practicum <- practicum_raw |>
  dplyr::mutate(
    specialty = dplyr::case_match(
      specialty,
      "ER VISIT" ~ "Emergency Medicine", # 23
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
      "INPATIENT" ~ "Inpatient", # 21
      "PSYCH" ~ "Behavioral Health",
      c("GENITOU", "GENITO") ~ "Genitourinary",
      "OSTEO" ~ "Osteopathy",
      "ENDOCR" ~ "Endocrinology",
      c("OPTIC", "OPTHAM") ~ "Opthamology",
      "WORKCMP" ~ "Worker's Comp"
    ) |>
      forcats::as_factor(),
    dplyr::across(
      c(
        diagnosis,
        icd,
        procedure,
        cpt,
        mod1,
        mod2,
        mod3,
        notes
        ),
      ~dplyr::na_if(.x, "-")
      )
    ) |>
  dplyr::rename(hcpcs = cpt) |>
  dplyr::arrange(group, case) |>
  dplyr::mutate(
    claim = dplyr::consecutive_id(case),
    group = NULL,
    case = NULL,
    .before = 1
  )

examples <- list(
  report = report,
  practicum = practicum
)

# Update pin -------------------
pin_update(
  examples,
  name = "examples",
  title = "Northstar Example Data",
  description = "Northstar Example Data"
)

# Example usage ----------------
practicum <- get_pin("practicum")

vctrs::vec_set_difference(
  collapse::funique(practicum$icd),
  icd_vec
)

bench::mark(
  collapse = collapse::funique(collapse::na_rm(practicum$icd)),
  collapse_base = collapse::funique(practicum$icd[!is.na(practicum$icd)]),
  vec_unique = vctrs::vec_unique(practicum$icd[!is.na(practicum$icd)]),
  base = unique(practicum$icd[!is.na(practicum$icd)]),
  dplyr = practicum |> dplyr::filter(!is.na(icd)) |> dplyr::distinct(icd) |> dplyr::pull(icd),
  min_iterations = 100
)

# ICD Codes -> Descriptions
icd_desc <- pathologie::icd10cm(icd = collapse::funique(collapse::na_rm(practicum$icd)))

practicum |>
  filter(!is.na(icd)) |>
  mutate(icd_ord = row_number(), .by = claim) |>
  select(claim, icd_ord, icd) |>
  left_join(icd_desc |> select(icd_code, icd_description), by = join_by(icd == icd_code), relationship = "many-to-many")

# HCPCS Codes -> Descriptions
hcpcs_desc <- northstar::search_descriptions(hcpcs_code = collapse::funique(collapse::na_rm(practicum$hcpcs)))

# Deleted HCPCS Codes
vctrs::vec_set_difference(
  collapse::funique(practicum$hcpcs[!is.na(practicum$hcpcs)]),
  hcpcs_desc$hcpcs_code
)

practicum |>
  filter(!is.na(hcpcs)) |>
  mutate(
    hcpcs_ord = row_number(),
    .by = claim
  ) |>
  select(
    claim,
    hcpcs_ord,
    hcpcs:mod3
    ) |>
  left_join(
    hcpcs_desc |>
      filter(hcpcs_desc_type == "Long") |>
      select(hcpcs_code, hcpcs_description),
    by = join_by(hcpcs == hcpcs_code),
    relationship = "many-to-many"
  )


# MUE Analysis
mue_vec <- practicum |>
  filter(!is.na(units)) |>
  count(hcpcs, sort = TRUE) |>
  pull(hcpcs)

mue_desc <- northstar::search_mues(hcpcs_code = mue_vec)

practicum |>
  filter(!is.na(units)) |>
  select(id, hcpcs, units) |>
  left_join(mue_desc, by = join_by(hcpcs == hcpcs_code)) |>
  mutate(above_mue_thresh = units > mue_units) |>
  filter(above_mue_thresh)

# Modifiers Analysis
mod_vec <- practicum |>
  filter(!is.na(hcpcs), !is.na(mod1)) |>
  select(id, hcpcs, mod1, mod2, mod3) |>
  pivot_longer(
    cols = starts_with("mod"),
    names_prefix = "mod",
    names_to = "order",
    values_to = "mod",
    values_drop_na = TRUE
  ) |>
  count(mod, sort = TRUE) |>
  pull(mod)

mod_desc <- search_modifiers(mod_code = mod_vec)

practicum |>
  filter(!is.na(hcpcs), !is.na(mod1)) |>
  select(id, hcpcs, mod1, mod2, mod3) |>
  pivot_longer(
    cols = starts_with("mod"),
    names_prefix = "mod",
    names_to = "order",
    values_to = "mod",
    values_drop_na = TRUE
  )

practicum |>
  filter(!is.na(hcpcs)) |>
  select(id, hcpcs, units, mod1, mod2, mod3) |>
  tidyr::unite(
    "mod",
    mod1:mod3,
    sep = "-",
    remove = TRUE,
    na.rm = TRUE
    ) |>
  mutate(mod = na_if(mod, "")) |>
  count(mod, sort = TRUE) |>
  print(n = 50)

hcpcs_desc <- northstar::describe_hcpcs(
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
