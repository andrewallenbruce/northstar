library(tidyverse)
library(rvest)
library(icd10us)

# base <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2024"
#
# x <- session(base) |>
#   session_follow_link("2024") |>
#   html_elements("a") |>
#   html_attr("href")
#
# x[2:8]

## Load 2024 version
icdcodes <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2024/icd10cm-CodesDescriptions-2024.zip"

download.file(url      = icdcodes,
              destfile = "data-raw/2024_Code_Descriptions.zip",
              method   = "libcurl")


unzip(
  "data-raw/2024_Code_Descriptions.zip",
  # list    = TRUE,
  files     = c("icd10cm-order-2024.txt"),
  exdir     = "data-raw",
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
  col_types = c("i", "c", "l", "c", "c"))

icd10 <- icd10cm |>
  select(
    order       = order_number,
    code        = icd10cm_code,
    valid       = valid_billing_code,
    description = icd10cm_long_description) |>
  mutate(valid  = as.integer(valid)) |>
  mutate(code   = add_dot(code)) |>
  case_chapter_icd10(code) |>
  select(order, valid, code, description, chapter)

chapters <- icd10us::icd10cm_chapters |>
  select(ch      = chapter_num,
         abbrev  = chapter_abbr,
         chapter = chapter_desc,
         start   = code_start,
         end     = code_end) |>
  mutate(range   = paste0(start, " - ", end),
         ch      = as.integer(ch)) |>
  select(-start, -end)


icd_join <- icd10 |>
  left_join(chapters, by = join_by(chapter))

icd_chapters <- icd_join |>
  group_by(ch, chapter) |>
  summarise(ch_start = min(order)) |>
  ungroup() |>
  mutate(ch_end = lead(ch_start) - 1,
         ch_end = if_else(ch_start == 97292, 97296, ch_end),
         ch_codes = ch_end - ch_start,
         ch_end = as.integer(ch_end),
         ch_codes = as.integer(ch_codes))

icd_sections <- icd_join |>
  select(order, valid, code, description) |>
  filter(valid == 0, stringr::str_length(code) == 3L) |>
  mutate(sc_start = order,
         sc_end = lead(order) - 1,
         sc_end = if_else(order == 97295, 97296, sc_end),
         sc_end = as.integer(sc_end),
         sc_codes = sc_end - sc_start) |>
  select(order,
         valid,
         code,
         section = description,
         sc_start,
         sc_end,
         sc_codes)

icd_nest <- icd_join |>
  left_join(icd_sections) |>
  fill(section, sc_start, sc_end, sc_codes) |>
  left_join(icd_chapters) |>
  nest(codes = c(order, valid, code, description)) |>
  mutate(ch_sc = dplyr::n_distinct(section), .by = chapter) |>
  nest(sections = c(section, codes, sc_start, sc_end, sc_codes)) |>
  select(ch,
         abbrev,
         chapter,
         range,
         ch_start,
         ch_end,
         ch_codes,
         ch_sc,
         sections)

fs::file_delete(fs::path("data-raw", c("icd10cm-order-2024.txt", "2024_Code_Descriptions.zip")))

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(icd_nest,
                  name = "icd10cm",
                  title = "2024 ICD-10-CM",
                  description = "2024 ICD-10-CM Codes and Descriptions",
                  type = "qs")

board |> pins::write_board_manifest()
