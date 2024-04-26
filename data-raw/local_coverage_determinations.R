source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

lcd <- lcd_paths |>
  map(read_csv, col_types = "c") |>
  map(fuimus::df_2_chr)

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
pin_update(
  v1,
  name = "lcd",
  title = "LCD Download Database Last Updated: 2023-04-27"
)
