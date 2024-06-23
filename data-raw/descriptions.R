source(here::here("data-raw", "source_setup", "setup.R"))

cpt_desc <- get_pin("cpt_descriptors")
two_desc <- get_pin("two_descriptions")

# From pfs_rvu_file
rvu24_jul |> dplyr::select(hcpcs_code, hcpcs_description)


# EDA ------------------
cpt_uq <- collapse::funique(cpt_desc$hcpcs) # 10641
two_uq <- collapse::funique(two_desc$hcpcs) # 7966
rvu_uq <- collapse::funique(rvu_desc$hcpcs) # 16325

length(c(rvu_uq, cpt_uq, two_uq)) # 34932

# Neither are in the other group - Total unique 18607
cpt_two_uq <- c(cpt_uq, two_uq)

# RVU & CPT - 10557
vctrs::vec_set_intersect(rvu_uq, cpt_uq) |> length()
# CPT only - 84
cpt_only <- vctrs::vec_set_difference(cpt_uq, rvu_uq)

cpt_desc |>
  filter(hcpcs %in% cpt_only)

# RVU & HCPCS - 4793
vctrs::vec_set_intersect(rvu_uq, two_uq) |> length()
# HCPCS only - 3173
hcpcs_only <- vctrs::vec_set_difference(two_uq, rvu_uq)

vctrs::vec_set_difference(rvu_uq, cpt_uq) |> length() # 5768
vctrs::vec_set_difference(rvu_uq, two_uq) |> length() # 11532

# RVU Only - 975
rvu_only <- vctrs::vec_set_difference(rvu_uq, cpt_two_uq)

rvu_desc |>
  filter(hcpcs %in% rvu_only)


# Combine Description Sets ---------------------
rvu_clean <- rvu_desc |>
  filter(hcpcs != "\032") |>
  rename(description = rel_desc) |>
  mutate(desc_type = "Short")

two_pivot <- two_desc |>
  select(
    hcpcs,
    Short = two_desc_short,
    Long = two_desc_long
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

cpt_pivot <- cpt_desc |>
  unnest(cpt_desc_clin) |>
  select(
    hcpcs,
    Short = cpt_desc_short,
    Long = cpt_desc_long,
    Medical = cpt_desc_med,
    Consumer = cpt_desc_cons,
    Clinician = cpt_desc_clin
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

hcpcs_desc <- rvu_clean |>
  dplyr::full_join(two_pivot) |>
  dplyr::full_join(cpt_pivot) |>
  dplyr::arrange(hcpcs) |>
  dplyr::distinct(
    hcpcs,
    description,
    .keep_all = TRUE)

short_dupes <- hcpcs_desc |>
  dplyr::filter(desc_type == "Short") |>
  janitor::get_dupes(hcpcs, desc_type) |>
  dplyr::count(hcpcs) |>
  dplyr::pull(hcpcs)

desc_dupes <- hcpcs_desc |>
  filter(hcpcs %in% short_dupes) |>
  filter(desc_type == "Short") |>
  group_by(hcpcs) |>
  slice_tail() |>
  ungroup() |>
  pull(description)

hcpcs_desc <- hcpcs_desc |>
  filter(!description %in% desc_dupes)

# Sanity check --------------------------
hcpcs_desc |>
  count(hcpcs) |>
  select(hcpcs) |>
  # filter(hcpcs %in% cpt_uq) # 10641
  # filter(hcpcs %in% two_uq) # 7966
  filter(hcpcs %in% rvu_uq) # 16324
# ---------------------------------------

# Add CPT PLA Codes
library(codexchain)

cpt_pla_codes <- codexchain::placodes |>
  select(
    hcpcs = X1,
    desc_long = X2,
    desc_medium = X3,
    desc_short = X4,
    X5,
    X6,
    date_released = X7,
    date_effective = X8,
    proprietary_name = X9,
    clinical_lab = X10,
    manufacturer = X11
  ) |>
  janitor::remove_empty()

pla_pivot <- cpt_pla_codes |>
  select(
    hcpcs,
    Short = desc_short,
    Medium = desc_medium,
    Long = desc_long,
    "Proprietary Name" = proprietary_name
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

hcpcs_desc <- vctrs::vec_rbind(
  hcpcs_desc,
  pla_pivot
)

# A tibble: 5 × 2
# end_letter     n
# 1 A            142
# 2 F            565
# 3 M             65
# 4 T           2224
# 5 U           1348


# Remove CPT M & T Codes with no descriptions
hcpcs_desc_rowid <- hcpcs_desc |>
  mutate(rowid = row_number())

m_codes_no_clin_desc <- hcpcs_desc_rowid |>
  filter(str_ends(hcpcs, regex("[M]"))) |>
  filter(is.na(description)) |>
  pull(rowid)

t_codes_no_clin_desc <- hcpcs_desc_rowid |>
  filter(str_ends(hcpcs, regex("[T]"))) |>
  filter(is.na(description)) |>
  pull(rowid)

rows_no_clin_desc <- c(m_codes_no_clin_desc, t_codes_no_clin_desc)

hcpcs_desc <- hcpcs_desc_rowid |>
  filter(!rowid %in% rows_no_clin_desc) |>
  select(-rowid)

# Add Levels and Categories ---------------------
hcpcs_desc <- hcpcs_desc |>
  mutate(level = case_when(
    str_detect(hcpcs, regex("^\\d{4}[A-Z0-9]$")) ~ "I",
    str_detect(hcpcs, regex("^[A-V]\\d{4}$")) ~ "II",
    .default = NA_character_
  )) |>
  mutate(category = case_when(
    level == "I" & str_ends(hcpcs, regex("[A-EG-SU-Z0-9]")) ~ "I",
    level == "I" & str_ends(hcpcs, regex("[F]")) ~ "II",
    level == "I" & str_ends(hcpcs, regex("[T]")) ~ "III",
    .default = NA_character_
  ))


# Add Sections and Ranges ---------------------
hcpcs_desc <- hcpcs_desc |>
  mutate(section = case_match(
    hcpcs,

    # Level I Codes
    as.character(99202:99499) ~ "Evaluation and Management",
    c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140) ~ "Anesthesiology",
    as.character(10004:69990) ~ "Surgery",
    as.character(70010:79999) ~ "Radiology",
    as.character(80047:89398) ~ "Pathology and Laboratory",
    as.character(c(90281:99199, 99500:99607)) ~ "Medicine",

    ## Category I Immunization Codes (ends in A)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "A") ~ "Immunization",

    ## Category I Administrative MAAA Codes
    ## (Multianalyte Assays With Algorithmic Analyses Codes) (ends in M)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "M") ~ "Administrative Multianalyte Assay With Algorithmic Analysis",

    ## Category I Proprietary Laboratory Analyses (PLA) Codes (ends in U)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "U") ~ "Proprietary Laboratory Analysis",

    ## Category II Performance Measurement Codes (ends in F)
    paste0(stringr::str_pad(1:15, width = 4, pad = "0"), "F") ~ "Composite Measures",
    paste0(stringr::str_pad(500:584, width = 4, pad = "0"), "F") ~ "Patient Management",
    paste0(stringr::str_pad(1000:1505, width = 4, pad = "0"), "F") ~ "Patient History",
    paste0(stringr::str_pad(2000:2060, width = 4, pad = "0"), "F") ~ "Physical Examination",
    paste0(stringr::str_pad(3006:3776, width = 4, pad = "0"), "F") ~ "Diagnostic/Screening Processes or Results",
    paste0(stringr::str_pad(4000:4563, width = 4, pad = "0"), "F") ~ "Therapeutic, Preventive or Other Interventions",
    paste0(stringr::str_pad(5005:5250, width = 4, pad = "0"), "F") ~ "Follow-Up or Other Outcomes",
    paste0(stringr::str_pad(6005:6150, width = 4, pad = "0"), "F") ~ "Patient Safety",
    paste0(stringr::str_pad(7010:7025, width = 4, pad = "0"), "F") ~ "Structural Measures",
    paste0(stringr::str_pad(9001:9007, width = 4, pad = "0"), "F") ~ "Non-Measure Claims Based Reporting",

    ## Category III Temporary Codes (ends in T)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "T") ~ "Temporary Codes",

    # Level II Codes

    paste0("A", stringr::str_pad(21:999, width = 4, pad = "0")) ~ "Transportation Services Including Ambulance",
    paste0("A", stringr::str_pad(2000:9999, width = 4, pad = "0")) ~ "Medical and Surgical Supplies",

    paste0("B", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Enteral and Parenteral Therapy",
    paste0("C", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Outpatient PPS",
    paste0("D", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Dental Codes",
    paste0("E", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Durable Medical Equipment",
    paste0("G", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Procedures/Professional Services (Temporary)",

    paste0("H", stringr::str_pad(1:2037, width = 4, pad = "0")) ~ "Alcohol and Drug Abuse Treatment Services",
    paste0("H", stringr::str_pad(2038:2041, width = 4, pad = "0")) ~ "Rehabilitative Services",

    paste0("J", stringr::str_pad(120:8499, width = 4, pad = "0")) ~ "Drugs Administered Other Than Oral Method",
    paste0("J", stringr::str_pad(8501:9999, width = 4, pad = "0")) ~ "Chemotherapy Drugs",

    paste0("K", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Temporary DME Codes",
    paste0("L", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Orthotic Procedures and Devices",

    paste0("M", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Medical Services/Quality Measures",

    # paste0("M", stringr::str_pad(75:301, width = 4, pad = "0")) ~ "Medical Services",
    # paste0("M", stringr::str_pad(1003:1149, width = 4, pad = "0")) ~ "Quality Measures",

    paste0("P", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Pathology and Laboratory",
    paste0("Q", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Miscellaneous Services (Temporary)",
    paste0("R", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Diagnostic Radiology Services",
    paste0("S", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Commercial Payers (Temporary)",
    paste0("T", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "State Medicaid Agency Codes",
    paste0("U", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Coronavirus Lab Tests",

    paste0("V", stringr::str_pad(2020:2799, width = 4, pad = "0")) ~ "Vision Services",
    paste0("V", stringr::str_pad(5008:5361, width = 4, pad = "0")) ~ "Hearing Services",
    paste0("V", stringr::str_pad(5362:5364, width = 4, pad = "0")) ~ "Speech-Language Pathology Services",

    .default = NA_character_
    )
  )

# Add RBCS ------------------------------
rbcs <- search_rbcs(concatenate = FALSE) |>
  filter(rbcs_date_hcpcs_end > lubridate::today()) |>
  select(
    hcpcs,
    rbcs_procedure,
    rbcs_category,
    rbcs_subcategory,
    rbcs_family
  )

rbcs_3 <- rbcs |>
  count(hcpcs, sort = TRUE) |>
  filter(n == 3) |>
  pull(hcpcs)

rbcs_2 <- rbcs |>
  count(hcpcs, sort = TRUE) |>
  filter(n == 2) |>
  pull(hcpcs)

rbcs_1 <- rbcs |>
  count(hcpcs, sort = TRUE) |>
  filter(n == 1) |>
  pull(hcpcs)

rbcs_3grp <- rbcs |>
  filter(hcpcs %in% rbcs_3) |>
  mutate(rbcs_family = na_if(rbcs_family, "No RBCS Family")) |>
  filter(!is.na(rbcs_family)) |>
  unite(
    "rbcs_category",
    c(rbcs_procedure, rbcs_category),
    sep = " ") |>
  unite(
    "rbcs_family",
    c(rbcs_subcategory, rbcs_family),
    sep = ": ",
    na.rm = TRUE) |>
  select(
    hcpcs,
    rbcs_category,
    rbcs_family)

rbcs_2grp <- rbcs |>
  filter(hcpcs %in% rbcs_2) |>
  mutate(rbcs_family = na_if(rbcs_family, "No RBCS Family")) |>
  filter(!is.na(rbcs_family)) |>
  unite(
    "rbcs_category",
    c(rbcs_procedure, rbcs_category),
    sep = " ") |>
  unite(
    "rbcs_family",
    c(rbcs_subcategory, rbcs_family),
    sep = ": ",
    na.rm = TRUE) |>
  select(
    hcpcs,
    rbcs_category,
    rbcs_family)

rbcs_1grp <- rbcs |>
  filter(hcpcs %in% rbcs_1) |>
  mutate(rbcs_family = na_if(rbcs_family, "No RBCS Family")) |>
  unite(
    "rbcs_category",
    c(rbcs_procedure, rbcs_category),
    sep = " ") |>
  unite(
    "rbcs_family",
    c(rbcs_subcategory, rbcs_family),
    sep = ": ",
    na.rm = TRUE) |>
  select(
    hcpcs,
    rbcs_category,
    rbcs_family)

rbcs_desc <- vctrs::vec_rbind(
  rbcs_3grp,
  rbcs_2grp,
  rbcs_1grp
)

hcpcs_desc <- hcpcs_desc |>
  left_join(rbcs_desc,
            by = join_by(hcpcs),
            relationship = "many-to-many")


pin_update(
  hcpcs_desc,
  name = "hcpcs_descriptions",
  title = "HCPCS Descriptions Master File",
  description = "HCPCS Descriptions Master File"
)
