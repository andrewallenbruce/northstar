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
  col_types = c("i", "c", "l", "c", "c")) |>
  select(
    order       = order_number,
    code        = icd10cm_code,
    valid       = valid_billing_code,
    description = icd10cm_long_description) |>
  mutate(valid  = as.integer(valid)) |>
  mutate(code   = add_dot(code))

icd10cm <- icd10cm |>
  case_chapter_icd10(code) |>
  select(order, valid, code, description, chapter)

chapters <- icd10us::icd10cm_chapters |>
  select(ch = chapter_num,
         abbrev = chapter_abbr,
         chapter = chapter_desc,
         start = code_start,
         end = code_end) |>
  mutate(range = paste0(start, " - ", end),
         ch = as.integer(ch)) |>
  select(-start, -end)


icd_join <- icd10cm |>
  left_join(chapters, by = join_by(chapter)) |>
  nest(codes = c(order, valid, code, description)) |>
  select(ch, abbrev, chapter, range, codes)

fs::file_delete(fs::path("data-raw", c("icd10cm-order-2024.txt", "2024_Code_Descriptions.zip")))

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(icd_join,
                  name = "icd10cm",
                  title = "2024 ICD-10-CM",
                  description = "2024 ICD-10-CM Codes and Descriptions",
                  type = "qs")

board |> pins::write_board_manifest()


# Sections
icd10cm_sections <- icd10cm() |>
  unnest(codes) |>
  select(order, valid, code, description) |>
  filter(valid == 0, stringr::str_length(code) == 3L) |>
  mutate(start = order,
         end = lead(order),
         end = if_else(order == 97295, 97296, end),
         end = as.integer(end),
         n = end - start,
         regex = paste0("^[", code, "]")) |>
  select(section = code,
         description,
         start,
         end,
         n,
         regex)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(icd10cm_sections,
                  name = "icd_sections",
                  title = "2024 ICD-10-CM Sections",
                  description = "2024 ICD-10-CM Sections",
                  type = "qs")

board |> pins::write_board_manifest()

# Subsections
icd10cm() |>
  unnest(codes) |>
  select(order, valid, code, description) |>
  filter(valid == 0, stringr::str_length(code) > 3L)
