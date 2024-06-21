source(here::here("data-raw", "source_setup", "setup.R"))

url_to_scrape <- "https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits"

aoc_urls <- read_html(url_to_scrape) |>
  html_elements("a") |>
  html_attr("href") |>
  unique() |>
  str_subset("/files/zip/")

latest_zip_url <- glue::glue("https://www.cms.gov{aoc_urls}")[1]

curl::multi_download(latest_zip_url)

xlsx_filename <- zip::zip_list(
  fs::dir_ls(glob = "*.zip")) |>
  filter(str_detect(filename, ".xlsx")) |>
  pull(filename)

zip::unzip(fs::dir_ls(glob = "*.zip"), files = xlsx_filename)

aoc <- here::here(xlsx_filename) |>
  map(read_excel, col_types = "text") |>
  map(fuimus::df_2_chr) |>
  pluck(1) |>
  clean_names() |>
  reframe(
    primary_code,
    add_on_code,
    aoc_edit_type = as.integer(aoc_edit_type),
    special_instruction_notes = str_remove_all(special_instruction_notes, regex('\\(|\\)|\\"')),
    aoc_del_dt_julian = substr(aoc_del_dt_julian, 1, 4) |> as.integer(),
    primary_code_del_dt_julian = substr(primary_code_del_dt_julian, 1, 4) |> as.integer(),
    aoc_edit_eff_dt_julian = substr(aoc_edit_eff_dt_julian, 1, 4) |> as.integer(),
    aoc_edit_del_dt_julian = substr(aoc_edit_del_dt_julian, 1, 4) |> as.integer()) |>
  filter(is.na(primary_code_del_dt_julian)) |>
  filter(is.na(aoc_edit_del_dt_julian)) |>
  fuimus::remove_quiet() |>
  select(-special_instruction_notes,
         -aoc_edit_eff_dt_julian)

fs::file_delete(here::here(fs::dir_ls(glob = "*.zip|*.xlsx")))

# special_instruction_notes:
# CCCCC = Contractor Defined Primary Codes
# 96376 = 96376 may be reported by facilities only
# 96360 = 96360 Practitioner Only
# 69990 = IOM: Pub. 100-04, Ch. 12, Sec. 20.4.5 Allowable Adjustments

primary <- aoc |>
  dplyr::reframe(
    hcpcs_code = primary_code,
    aoc_type = "Primary",
    aoc_complement = add_on_code,
    aoc_edit_type)

addon <- aoc |>
  dplyr::reframe(
    hcpcs_code = add_on_code,
    aoc_type = "Add-On",
    aoc_complement = primary_code,
    aoc_edit_type)

aoc_nest <- vctrs::vec_rbind(addon, primary) |>
  tidyr::nest(aoc_complements = c(aoc_complement)) |>
  dplyr::reframe(
    hcpcs_code,
    aoc_type = forcats::as_factor(aoc_type),
    aoc_complements,
    aoc_edit = aoc_edit_type) |>
  dplyr::arrange(hcpcs_code)

aoc
aoc_long

# Update Pin
pin_update(
  aoc_long,
  name        = "ncci_aoc_nested",
  title       = "Add-on Code Edits (Nested)",
  description = "NCCI Add-on Code Edits 2024-06-03")

pin_update(
  aoc,
  name        = "ncci_aoc",
  title       = "Add-on Code Edits",
  description = "NCCI Add-on Code Edits 2024-06-03")
