library(tidyverse)
library(janitor)
library(gt)

paths <- fs::dir_ls("C:/Users/Andrew/Desktop/all_data/all_lcd/all_lcd_csv/", regexp = "*.csv$")
names <- paths |> basename() |> str_remove_all(pattern = fixed(".csv"))
names(paths) <- names

df2chr <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), as.character))
}

lcd <- paths |>
  map(read_csv, col_types = "c") |>
  map(df2chr)

contractor <- lcd$state_x_region |>
  left_join(lcd$state_lookup |> rename(state_description = description)) |>
  left_join(lcd$region_lookup |> rename(region_description = description)) |>
  left_join(lcd$contractor_jurisdiction |> rename(contractor_jurisdiction_last_updated = last_updated,
                                                  contractor_jurisdiction_active_date = active_date,
                                                  contractor_jurisdiction_term_date = term_date)) |>
  left_join(lcd$contractor_oversight) |>
  left_join(lcd$dmerc_region_lookup |> rename(dmerc_region_description = description)) |>
  left_join(lcd$contractor) |>
  left_join(lcd$contractor_type_lookup |> rename(contractor_type_description = description)) |>
  left_join(lcd$contractor_subtype_lookup |> rename(contractor_subtype_description = description))

lcds <- lcd$lcd |>
  left_join(lcd$draft_contact_lookup |> rename(draft_contact = contact_id, draft_last_updated = last_updated)) |>
  left_join(lcd$lcd_future_retire) |>
  left_join(lcd$lcd_related_documents) |>
  left_join(lcd$lcd_related_ncd_documents) |>
  left_join(lcd$lcd_related_source_icd9)

urls <- lcd$lcd_x_urls |> left_join(lcd$lcd_url_type_lookup)

v1 <- lcd$lcd_x_hcpc_code |>
  left_join(lcd$lcd_x_hcpc_code_group) |>
  left_join(lcds) |>
  left_join(urls)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(v1,
                  name = "lcd",
                  title = "LCD Download Database Last Updated: 2023-04-27",
                  type = "qs")

board |> pins::write_board_manifest()
