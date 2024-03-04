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

cpt_desc <- left_join(clin,
          cons,
          by = join_by(concept_id, cpt_code)) |>
  select(cpt = cpt_code,
         clinician_descriptor,
         consumer_descriptor = consumer_friendly_descriptor)

# E/M Codes

em <- clin |>
  select(cpt = cpt_code,
         description = clinician_descriptor) |>
  filter(cpt %in% c(as.character(99202:99215))) |>
  unite("description", cpt:description, sep = ": ", remove = TRUE)

pattern <- c("{code}: Outpatient visit for evaluation and management of {patient} patient, including medically appropriate {ex_hist} and {mdm} medical decision making, total time {minutes} minutes")

unglue::unglue_data(em$description, pattern) |>
  dplyr::tibble() |>
  print(n = Inf)



# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(cpt_desc,
                  name = "cpt_descriptors",
                  title = "CPT Clinician & Consumer-Friendly Descriptors 2023",
                  type = "qs")

board |> pins::write_board_manifest()
