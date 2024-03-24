library(readxl)
library(tidyverse)
library(janitor)

level2 <- "C:/Users/Andrew/Desktop/payer_guidelines/data/HCPC2024_JAN_ANWEB_v4/HCPC2024_JAN_ANWEB_v4.xlsx"

two <- read_excel(level2, col_types = "text") |>
  clean_names() |>
  remove_empty() |>
  rename(hcpcs = hcpc) |>
  mutate(len   = str_length(hcpcs), .after = hcpcs) |>
  mutate(type  = if_else(len == 2, "mod", "code"), .after = hcpcs) |>
  mutate(len   = NULL) |>
  mutate(add_dt     = convert_to_date(add_dt),
         act_eff_dt = convert_to_date(act_eff_dt),
         term_dt    = convert_to_date(term_dt),
         asc_dt     = convert_to_date(asc_dt)) |>
  select(-c(seqnum, recid, anest_bu)) |>
  unite("price", price1:price2, sep = ":", remove = TRUE, na.rm = TRUE) |>
  unite("cim", cim1:cim2, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("mcm", mcm1:mcm3, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("labcert", labcert1:labcert4, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("xref", xref1:xref2, sep = ", ", remove = TRUE, na.rm = TRUE) |>
  unite("tos", tos1:tos4, sep = ":", remove = TRUE, na.rm = TRUE) |>
  mutate(price       = na_if(price, "")) |>
  mutate(cim         = na_if(cim, "")) |>
  mutate(mcm         = na_if(mcm, "")) |>
  mutate(labcert     = na_if(labcert, "")) |>
  mutate(xref        = na_if(xref, "")) |>
  mutate(tos         = na_if(tos, "")) |>
  rename(date_added  = add_dt,
         date_action = act_eff_dt,
         date_ended  = term_dt,
         action      = action_cd)

two |>
  count(action) |> print(n = Inf)


two <- pins::pin_read(mount_board(), "hcpcs") |>
  dplyr::rename(description_short = short_description,
                description_long = long_description,
                asc = asc_grp,
                coverage = cov,
                multi_price = mult_pi)


board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(two,
                  name = "hcpcs",
                  title = "2024 HCPCS Level II",
                  description = "2024 Healthcare Common Procedure Coding System (HCPCS)",
                  type = "qs")

board |> pins::write_board_manifest()

