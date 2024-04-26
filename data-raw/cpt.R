source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

clin <- readxl::read_excel(clinician, col_types = "text") |>
  janitor::clean_names()

# 9,968 unique CPT codes
clin |> dplyr::distinct(cpt_code)

cons <- readxl::read_excel(consumer, col_types = "text") |>
  janitor::clean_names()

# 10,411 unique CPT codes
cons |> dplyr::distinct(cpt_code)

cpt_desc <- dplyr::left_join(
  cons,
  clin,
  by = dplyr::join_by(concept_id, cpt_code)
  ) |>
  dplyr::select(
    hcpcs = cpt_code,
    cpt_desc_clin = clinician_descriptor,
    description_consumer = consumer_friendly_descriptor
    ) |>
  tidyr::nest(descriptions_clinician = c(cpt_desc_clin))

cpt_desc <- cpt_desc |>
  dplyr::mutate(chapter = dplyr::case_match(
    hcpcs,
    as.character(99202:99499) ~ "Evaluation & Management",
    as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology",
    as.character(10004:69990) ~ "Surgery",
    as.character(70010:79999) ~ "Radiology",
    as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Pathology & Laboratory",
    as.character(c(90281:99199, 99500:99607)) ~ "Medicine",
    .default = NA_character_
    ),
  .after = hcpcs
  ) |>
  dplyr::mutate(range = dplyr::case_match(
    hcpcs,
    as.character(99202:99499) ~ "99202 - 99499",
    as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "00100 - 01999, 99100 - 99140",
    as.character(10004:69990) ~ "10004 - 69990",
    as.character(70010:79999) ~ "70010 - 79999",
    as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "80047 - 89398, 0001U - 0222U",
    as.character(c(90281:99199, 99500:99607)) ~ "90281 - 99199, 99500 - 99607",
    .default = NA_character_
  ),
  .after = chapter)

# 10,411 unique CPT codes
cpt_desc

# LONGUF File ------------------------------------
longuf <- readr::read_table(
  longuf,
  col_names = FALSE) |>
  dplyr::mutate(
    hcpcs   = substr(X1, 1, 5),
    id      = substr(X1, 6, 20),
    id      = as.integer(id),
    X1      = NULL,
    row     = dplyr::row_number(),
    .before = 1
    ) |>
  tidyr::unite(
    "hcpcs_description",
    X2:X13,
    sep     = " ",
    remove  = TRUE,
    na.rm   = TRUE)

row_ids <- longuf |>
  dplyr::filter(id == 1) |>
  dplyr::select(hcpcs, id, start = row)


hcpcs_longuf <- dplyr::left_join(
  longuf,
  row_ids,
  by = dplyr::join_by(hcpcs, id)
  ) |>
  tidyr::fill(start) |>
  dplyr::relocate(start, .before = id) |>
  dplyr::select(hcpcs, hcpcs_description) |>
  dplyr::group_by(hcpcs) |>
  tidyr::nest(description = c(hcpcs_description)) |>
  dplyr::rowwise() |>
  dplyr::mutate(hcpcs_description_longuf = purrr::map(
    description, ~paste0(., collapse = " "))) |>
  tidyr::unnest(cols = hcpcs_description_longuf) |>
  dplyr::ungroup() |>
  dplyr::select(hcpcs, hcpcs_description_longuf)

# 10,641 unique CPT codes
hcpcs_longuf

# LONGULF File ------------------------------------
longulf <- readr::read_table(
  longulf,
  col_names = FALSE) |>
  dplyr::mutate(
    hcpcs   = substr(X1, 1, 5),
    id      = substr(X1, 6, 20),
    id      = as.integer(id),
    X1      = NULL,
    row     = dplyr::row_number(),
    .before = 1
  ) |>
  tidyr::unite(
    "hcpcs_description",
    X2:X13,
    sep     = " ",
    remove  = TRUE,
    na.rm   = TRUE)

row_ids_ulf <- longulf |>
  dplyr::filter(id == 1) |>
  dplyr::select(hcpcs, id, start = row)

hcpcs_longulf <- dplyr::left_join(
  longulf,
  row_ids_ulf,
  by = dplyr::join_by(hcpcs, id)
) |>
  tidyr::fill(start) |>
  dplyr::relocate(start, .before = id) |>
  dplyr::select(hcpcs, hcpcs_description) |>
  dplyr::group_by(hcpcs) |>
  tidyr::nest(description = c(hcpcs_description)) |>
  dplyr::rowwise() |>
  dplyr::mutate(hcpcs_description_longulf = purrr::map(
    description, ~paste0(., collapse = " "))) |>
  tidyr::unnest(cols = hcpcs_description_longulf) |>
  dplyr::ungroup() |>
  dplyr::select(hcpcs, hcpcs_description_longulf)

# 10,641 unique CPT codes
hcpcs_longulf

# SHORTU File ------------------------------------
shortu <- readr::read_table(
  shortu,
  col_names = FALSE) |>
  dplyr::rename(hcpcs = X1) |>
  tidyr::unite(
    "hcpcs_description_shortu",
    X2:X7,
    sep     = " ",
    remove  = TRUE,
    na.rm   = TRUE)

# 10,641 unique CPT codes
shortu

# MEDU File ------------------------------------
medu <- readr::read_table(
  medu,
  col_names = FALSE) |>
  dplyr::rename(hcpcs = X1) |>
  tidyr::unite(
    "hcpcs_description_medu",
    X2:X7,
    sep     = " ",
    remove  = TRUE,
    na.rm   = TRUE)

# 10,641 unique CPT codes
medu

# Join #1 ------------------------------------
cpt_desc2 <- hcpcs_longuf |>
  dplyr::left_join(hcpcs_longulf, by = dplyr::join_by(hcpcs)) |>
  dplyr::left_join(shortu, by = dplyr::join_by(hcpcs)) |>
  dplyr::left_join(medu, by = dplyr::join_by(hcpcs)) |>
  dplyr::select(
    hcpcs,
    # cpt_desc_longuf = hcpcs_description_longuf,
    cpt_desc_long = hcpcs_description_longulf,
    cpt_desc_short = hcpcs_description_shortu,
    cpt_desc_med = hcpcs_description_medu
  )

# 10,641 unique CPT codes
cpt_desc2

# Join #2 ------------------------------------
cpt_desc2 <- cpt_desc2 |>
  dplyr::left_join(cpt_desc, by = dplyr::join_by(hcpcs)) |>
  dplyr::select(
    hcpcs,
    cpt_chapter = chapter,
    cpt_range = range,
    cpt_desc_cons = description_consumer,
    cpt_desc_long,
    cpt_desc_short,
    cpt_desc_med,
    cpt_desc_clin = descriptions_clinician
    )

# Should be 10,641 unique CPT codes
#
# Update Pin
pin_update(
  cpt_desc2,
  name = "cpt_descriptors",
  title = "CPT Clinician & Consumer-Friendly Descriptors 2023"
)
