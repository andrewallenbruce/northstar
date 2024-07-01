source(here::here("data-raw", "source_setup", "setup.R"))

# Category I Immunization Codes
# (A codes)
# https://www.ama-assn.org/practice-management/cpt/category-i-immunization-codes

# Category I Proprietary Laboratory Analyses (PLA) Codes
# (U codes)
# https://www.ama-assn.org/practice-management/cpt/cpt-pla-codes

# Category I Molecular Pathology Tier 2 Codes
# https://www.uhcprovider.com/content/dam/provider/docs/public/policies/exchange-reimbursement/VB-Exchange-Molecular-Pathology-Policy-Professional.pdf
# https://www.ama-assn.org/practice-management/cpt/molecular-pathology-tier-2-codes

# Category I Administrative MAAA Codes (Multianalyte Assays With Algorithmic Analyses Codes)
# (M codes)
# https://www.ama-assn.org/practice-management/cpt/multianalyte-assays-algorithmic-analyses-codes

# Category II Performance Measurement Codes
# (F codes)
# https://www.ama-assn.org/practice-management/cpt/category-ii-codes

# Category III Temporary Codes (T codes)
# https://www.ama-assn.org/practice-management/cpt/category-iii-codes

# Clinician Descriptors
cpt_clinician <- codexchain::clinician |>
  dplyr::reframe(hcpcs_code = cpt_code,
                 hcpcs_desc_type = "Clinician",
                 hcpcs_description = clinician_descriptor)

# cpt_clinician <- codexchain::clinician |>
#   dplyr::reframe(hcpcs_code = cpt_code,
#                  hcpcs_description = clinician_descriptor) |>
#   tidyr::nest(.by = hcpcs_code, .key = 'hcpcs_description') |>
#   dplyr::rowwise() |>
#   dplyr::mutate(hcpcs_description_type = "Clinician",
#                 hcpcs_description = purrr::map(hcpcs_description, paste0, collapse = " ")) |>
#   tidyr::unnest(cols = hcpcs_description)

# Consumer-Friendly Descriptors
cpt_consumer <- codexchain::consumer |>
  dplyr::reframe(hcpcs_code = cpt_code,
                 hcpcs_desc_type = "Consumer",
                 hcpcs_description = consumer_friendly_descriptor)

# LONGULF - lowercase
cpt_longulf <- codexchain::longulf |>
  dplyr::reframe(
    hcpcs_code = substr(X1, 1, 5),
    hcpcs_description = substr(X1, 10, 100)) |>
  tidyr::nest(
    .by = hcpcs_code,
    .key = 'hcpcs_description') |>
  dplyr::rowwise() |>
  dplyr::mutate(
    hcpcs_desc_type = "Long",
    hcpcs_description = purrr::map(
      hcpcs_description,
      paste0,
      collapse = " ")) |>
  tidyr::unnest(cols = hcpcs_description)

# LONGUF - uppercase
# cpt_longuf <- codexchain::longuf |>
#   dplyr::reframe(
#     hcpcs_code = substr(X1, 1, 5),
#     hcpcs_description = substr(X1, 10, 100)) |>
#   tidyr::nest(
#     .by = hcpcs_code,
#     .key = 'hcpcs_description') |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     hcpcs_desc_type = "Long UF",
#     hcpcs_description = purrr::map(
#       hcpcs_description,
#       paste0,
#       collapse = " ")) |>
#   tidyr::unnest(cols = hcpcs_description)

# LONGULT - lowercase
# cpt_longult <- codexchain::longult |>
#   dplyr::reframe(
#     hcpcs_code = X1,
#     hcpcs_description = X2,
#     hcpcs_desc_type = "Long ULT"
#   )

# SHORTU - uppercase
cpt_shortuf <- codexchain::shortu |>
  dplyr::reframe(
    hcpcs_code = substr(X1, 1, 5),
    hcpcs_desc_type = "Short",
    hcpcs_description = substr(X1, 7, 1000)
    )

# MEDU - uppercase
cpt_medu <- codexchain::medu |>
  dplyr::reframe(
    hcpcs_code = substr(X1, 1, 5),
    hcpcs_desc_type = "Medical",
    hcpcs_description = substr(X1, 7, 1000)
  )

# Concept IDs + CPTs
# cpt_concept_id <- codexchain::concept_id |>
#   dplyr::reframe(
#     hcpcs_code = X2,
#     cpt_id = as.integer(X1),
#   )

# Category I Proprietary Laboratory Analyses (PLA) Codes
cpt_pla <- codexchain::placodes |>
  dplyr::select(
    hcpcs_code = X1,
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
  fuimus::remove_quiet()

cpt_pla_desc <- cpt_pla |>
  dplyr::reframe(
    hcpcs_code,
    Long = desc_long,
    Short = desc_medium,
    "Proprietary Name" = proprietary_name
  ) |>
  tidyr::pivot_longer(
    cols = !hcpcs_code,
    names_to = "hcpcs_desc_type",
    values_to = "hcpcs_description"
  )

# cpt_desc |>
#   dplyr::mutate(chapter = dplyr::case_match(
#     hcpcs,
#     as.character(99202:99499) ~ "Evaluation & Management",
#     as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology",
#     as.character(10004:69990) ~ "Surgery",
#     as.character(70010:79999) ~ "Radiology",
#     as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Pathology & Laboratory",
#     as.character(c(90281:99199, 99500:99607)) ~ "Medicine",
#     .default = NA_character_
#     ),
#   .after = hcpcs
#   ) |>
#   dplyr::mutate(range = dplyr::case_match(
#     hcpcs,
#     as.character(99202:99499) ~ "99202 - 99499",
#     as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "00100 - 01999, 99100 - 99140",
#     as.character(10004:69990) ~ "10004 - 69990",
#     as.character(70010:79999) ~ "70010 - 79999",
#     as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "80047 - 89398, 0001U - 0222U",
#     as.character(c(90281:99199, 99500:99607)) ~ "90281 - 99199, 99500 - 99607",
#     .default = NA_character_
#   ),
#   .after = chapter)


cpt_desc <- vctrs::vec_rbind(
  cpt_clinician,
  cpt_consumer,
  cpt_longulf,
  cpt_shortuf,
  cpt_medu,
  cpt_pla_desc
) |>
  dplyr::arrange(hcpcs_code)

# Update Pin
pin_update(
  cpt_desc,
  name = "cpt_descriptions",
  title = "CPT Descriptors 2023",
  description = "CPT Descriptors 2023"
)

pin_update(
  cpt_pla,
  name        = "cpt_pla",
  title       = "Category I Proprietary Laboratory Analyses (PLA) Codes",
  description = "Category I Proprietary Laboratory Analyses (PLA) Codes"
)
