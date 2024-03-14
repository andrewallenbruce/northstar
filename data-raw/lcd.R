library(tidyverse)
library(janitor)
library(gt)

paths <- fs::dir_ls("C:/Users/Andrew/Desktop/all_data/all_lcd/all_lcd_csv/", regexp = "*.csv$")
names <- paths |> basename() |> str_remove_all(pattern = fixed(".csv"))
names(paths) <- names
lcd_files <- paths |> purrr::map(read_csv, col_types = "c")

lcd_files$lcd_x_urls |> print(n = Inf)

lcd_files$lcd_x_hcpc_code_group

lcd_files$lcd |>
  glimpse()

lcd_files$lcd_x_hcpc_code_group |>
  mutate(paragraph = str_remove_all(paragraph, "'"),
         paragraph = iconv(paragraph, from = "", to = "ASCII//TRANSLIT"),
         # last_updated = iconv(last_updated, from = "", to = "ASCII//TRANSLIT"),
         last_updated = convert_to_date(last_updated)) |>
  # select(last_updated) |> print(n = Inf)
  select(-last_updated) |>
  gt() |>
  fmt_markdown() |>
  gtExtras::gt_theme_nytimes()

lcd_files$lcd_x_hcpc_code

lcd_files$state_lookup
