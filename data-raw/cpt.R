library(readxl)
library(tidyverse)
library(janitor)

cpt       <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/cpt2023/")
clinician <- glue::glue("{cpt}ClinicianDescriptor.xlsx")
consumer  <- glue::glue("{cpt}ConsumerDescriptor.xlsx")
modifer   <- glue::glue("{cpt}MODUL.txt")
longuf    <- glue::glue("{cpt}LONGUF_edit.txt")
longulf   <- glue::glue("{cpt}LONGULF_edit.txt")
shortuf   <- glue::glue("{cpt}SHORTUF_edit.txt")
medu      <- glue::glue("{cpt}MEDU_edit.txt")

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
longuf_file    <- glue::glue("{cpt}LONGUF_edit.txt")

longuf <- readr::read_table(
  longuf_file,
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
longulf_file   <- glue::glue("{cpt}LONGULF_edit.txt")

longulf <- readr::read_table(
  longulf_file,
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
shortu_file <- glue::glue("{cpt}SHORTU_edit.txt")

shortu <- readr::read_table(
  shortu_file,
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
medu_file <- glue::glue("{cpt}MEDU_edit.txt")

medu <- readr::read_table(
  medu_file,
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
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(cpt_desc2,
                  name = "cpt_descriptors",
                  title = "CPT Clinician & Consumer-Friendly Descriptors 2023",
                  type = "qs")

board |> pins::write_board_manifest()

# E/M Codes
library(unglue)

`%chin%` <- data.table::`%chin%`

em <- descriptors() |>
  filter(cpt %chin% c(as.character(99202:99215))) |>
  select(cpt, clinician_descriptor) |>
  unite("code", cpt:clinician_descriptor, sep = ": ") |>
  # as.data.frame()
  pull(code)


library(inferregex)
infer_regex(c("I knew it!"))


pt1 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient with {mdm}")
pt2 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient, including medically appropriate {procedure} and {mdm} medical decision making, total time {begin}-{end} minutes")
pt3 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient, including medically appropriate {procedure} and {mdm} medical decision making, total time {begin}-{end}")

unglue_data(em, c(pt1, pt2, pt3)) |>
  dplyr::tibble() |>
  # unite("minutes", begin:end, sep = "-", na.rm = TRUE) |>
  mutate(
    # minutes = na_if(minutes, ""),
         type = stringr::str_to_title(type),
         procedure = stringr::str_to_title(procedure),
         mdm = stringr::str_remove(mdm, " level of"),
         mdm = stringr::str_to_title(mdm)) |>
  select(cpt,
         patient = type,
         procedure,
         medical_decision_making = mdm,
         from = begin,
         to = end) |>
  mutate(from = as.integer(from),
         to = as.integer(to),
         minutes = ivs::iv(from, to)) |>
  print(n = Inf)
