library(tidyverse)
library(rvest)
library(icd10us)

base <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2024"

x <- session(base) |>
  session_follow_link("2024") |>
  html_elements("a") |>
  html_attr("href")

x[2:8]

## Load 2024 version
icdcodes <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2024/icd10cm-CodesDescriptions-2024.zip"

download.file(url = icdcodes,
              destfile = "data-raw/2024_Code_Descriptions.zip",
              method = "libcurl")


unzip(
  "data-raw/2024_Code_Descriptions.zip",
  # list = TRUE,
  files = c("icd10cm-order-2024.txt"),
  exdir = "data-raw",
  junkpaths = TRUE
)



icd10cm <- read_fwf(
  "data-raw/icd10cm-order-2024.txt",
  fwf_cols(
    order_number              = c(1,5),
    icd10cm_code              = c(7,13),
    valid_billing_code        = c(14,15),
    icd10cm_short_description = c(17,77),
    icd10cm_long_description  = c(78,500)),
  col_types = c("i", "c", "l", "c", "c")) |>
  select(
    order       = order_number,
    code        = icd10cm_code,
    valid       = valid_billing_code,
    description = icd10cm_long_description) |>
  mutate(valid  = as.integer(valid)) |>
  mutate(code   = add_dot(code)) |>
  case_section_icd10(code) |>
  select(order, valid, code, description, section)

icd10cm |> count(section) |> print(n = Inf)

chapters <- icd10us::icd10cm_chapters |>
  select(chapter = chapter_num,
         abbreviation = chapter_abbr,
         section = chapter_desc,
         start = code_start,
         end = code_end) |>
  mutate(section = stringr::str_to_title(section),
         range = paste0(start, " - ", end),
         chapter  = as.integer(chapter))

icd10cm <- icd10cm |>
  left_join(chapters) |>
  nest(codes = c(order, valid, code, description)) |>
  select(chapter, abbreviation, section, start, end, range, codes)

board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(icd10cm,
                  name = "icd10cm",
                  title = "2024 ICD-10-CM",
                  description = "2024 ICD-10-CM Codes and Descriptions",
                  type = "qs")

board |> pins::write_board_manifest()

fs::file_delete(
  fs::path("data-raw",
           c("icd10cm-order-2024.txt", "2024_Code_Descriptions.zip")
           )
  )

dplyr::tibble(
  ch = 1:22,
  regex = c(
    "^[A-B]",
    "(^[C]|^[D][0-4])",
    "^[D][5-8]",
    "^[E]",
    "^[F]",
    "^[G]",
    "^[H][0-5]\\d{1}\\.?\\d?",
    "^[H][6-9]\\d{1}\\.?\\d?",
    "^[I]",
    "^[J]",
    "^[K]",
    "^[L]",
    "^[M]",
    "^[N]",
    "^[O]",
    "^[P]",
    "^[Q]",
    "^[R]",
    "^[S-T]",
    "^[V-Y]",
    "^[Z]",
    "^[U]"
  )
)
