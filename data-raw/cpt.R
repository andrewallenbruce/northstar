library(readxl)
library(tidyverse)
library(janitor)

cpt       <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/cpt2023/")
clinician <- glue::glue("{cpt}ClinicianDescriptor.xlsx")
consumer  <- glue::glue("{cpt}ConsumerDescriptor.xlsx")
modifer   <- glue::glue("{cpt}MODUL.txt")


clin <- read_excel(clinician, col_types = "text") |>
  clean_names()

cons <- read_excel(consumer, col_types = "text") |>
  clean_names()

cpt_desc <- left_join(clin, cons, by = join_by(concept_id, cpt_code)) |>
  select(hcpcs = cpt_code,
         description_clinician = clinician_descriptor,
         description_consumer = consumer_friendly_descriptor) |>
  nest(descriptions_clinician = c(description_clinician))


cpt_desc <- cpt_desc |>
  dplyr::mutate(chapter = dplyr::case_match(hcpcs,
    as.character(99202:99499) ~ "Evaluation & Management",
    as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "Anesthesiology",
    as.character(10004:69990) ~ "Surgery",
    as.character(70010:79999) ~ "Radiology",
    as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "Pathology & Laboratory",
    as.character(c(90281:99199, 99500:99607)) ~ "Medicine",
    .default = NA_character_
  ),
  .after = hcpcs) |>
  dplyr::mutate(range = dplyr::case_match(hcpcs,
  as.character(99202:99499) ~ "99202 - 99499",
  as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)) ~ "00100 - 01999, 99100 - 99140",
  as.character(10004:69990) ~ "10004 - 69990",
  as.character(70010:79999) ~ "70010 - 79999",
  as.character(c(80047:89398, stringr::str_pad(paste0(1:222, "U"), width = 5, pad = "0"))) ~ "80047 - 89398, 0001U - 0222U",
  as.character(c(90281:99199, 99500:99607)) ~ "90281 - 99199, 99500 - 99607",
  .default = NA_character_
  ),
  .after = chapter)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(cpt_desc,
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
