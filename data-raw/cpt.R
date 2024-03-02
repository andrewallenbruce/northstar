library(readxl)
library(tidyverse)
library(janitor)

cpt       <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/cpt2023/")
clinician <- glue::glue("{cpt}ClinicianDescriptor.xlsx")
consumer  <- glue::glue("{cpt}ConsumerDescriptor.xlsx")
modifer   <- glue::glue("{cpt}MODUL.txt")


clin <- read_excel(clinician, col_types = "text") |>
  clean_names()

em <- clin |>
  select(cpt = cpt_code,
         description = clinician_descriptor) |>
  filter(cpt %in% c(as.character(99202:99215))) |>
  unite("description", cpt:description, sep = ": ", remove = TRUE)

pattern <- c("{code}: Outpatient visit for evaluation and management of {patient} patient, including medically appropriate {ex_hist} and {mdm} medical decision making, total time {minutes} minutes")

unglue::unglue_data(em$description, pattern) |>
  dplyr::tibble() |>
  print(n = Inf)

cons <- read_excel(consumer, col_types = "text") |>
  clean_names()
