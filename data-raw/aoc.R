library(readxl)
library(tidyverse)
library(janitor)

# NCCI Files Updated Quarterly
paths <- fs::dir_ls("C:/Users/Andrew/Desktop/payer_guidelines/data/NCCI/", regexp = "*MCR.xlsx$")
names <- paths |> basename() |> str_remove_all(pattern = fixed(".xlsx"))
names(paths) <- names

df2chr <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), as.character))
}

ncci <- paths |>
  map(read_excel, col_types = "text") |>
  map(df2chr)

# Medicare NCCI Add-on Code Edits 2024-04-01
# https://www.cms.gov/ncci-medicare/medicare-ncci-add-code-edits

aoc <- ncci$`AOC_V2024Q2-MCR` |>
  clean_names() |>
  mutate(
    aoc_del_dt                = as.integer(substr(aoc_del_dt, 1, 4)),
    primary_code_del_dt       = as.integer(substr(primary_code_del_dt, 1, 4)),
    aoc_edit_eff_dt           = as.integer(substr(aoc_edit_eff_dt, 1, 4)),
    aoc_edit_del_dt           = as.integer(substr(aoc_edit_del_dt, 1, 4)),
    aoc_edit_type             = as.integer(aoc_edit_type),
    special_instruction_notes = str_remove_all(special_instruction_notes, regex('\\(|\\)|\\"')),
    # primary_code              = dplyr::if_else(primary_code == "CCCCC", NA_character_, primary_code),
    type_description = case_match(as.character(aoc_edit_type),
                                  "1" ~ "Only Paid if Primary is Paid. Payment Eligible if Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.",
                                  "2" ~ "No Specific Primary Codes. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS.",
                                  "3" ~ "Some Specific Primaries. Payment Eligible if, as Determined by MAC, Primary Payment Eligible to Same Practitioner for Same Patient on Same DOS."
    )) |>
  select(
    primary                   = primary_code,
    addon                     = add_on_code,
    type                      = aoc_edit_type,
    type_description,
    primary_deleted           = primary_code_del_dt,
    addon_deleted             = aoc_del_dt,
    edit_effective            = aoc_edit_eff_dt,
    edit_deleted              = aoc_edit_del_dt,
    notes                     = special_instruction_notes
  )

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    aoc,
    name = "aoc",
    title = "Add-on Code Edits",
    description = "Medicare NCCI Add-on Code Edits 2024-04-01",
    type = "qs"
  )

board |> pins::write_board_manifest()

# pins::pin_list(board)
# pins::pin_versions(board, "aoc")
# pins::pin_delete(board, "aoc")

primary <- search_addons() |>
  filter(!is.na(primary)) |>
  count(primary, sort = TRUE) |>
  pull(primary)

addon <- search_addons() |>
  filter(!is.na(addon)) |>
  count(addon, sort = TRUE) |>
  pull(addon)

# HCPCS that are both primary and addon codes
both <- intersect(primary, addon)
length(both) # 160

# HCPCS that are primary only
primary_only <- setdiff(primary, addon)
length(primary_only) # 2366

# HCPCS that are add-on only
addon_only <- setdiff(addon, primary)
length(addon_only) # 583

aoc_vecs <- list(
  both    = both, # 160
  addon   = addon_only, # 583
  primary = primary_only # 2365
)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    aoc_vecs,
    name        = "aoc_vecs",
    title       = "Add-On Code Vectors",
    description = "List of 3 Vectors of AOCs: Both, Add-On Only, Primary Only",
    type = "qs"
  )

board |> pins::write_board_manifest()

# HCPCS that are either primary or add-on, not both
union(primary, addon) |> length() # 3109 + 160 = 3269

# HCPCS union + intersection (total)
c(addon, primary) |> length() # 3269

c(addon, primary) %in% addon[sample(1:length(addon), 10)]

primary %in% addon[sample(1:length(addon), 10)]

addon[collapse::fmatch(addon[sample(1:length(addon), 10)], addon, nomatch = 0)]

addon[sample(1:length(addon), 10)]

primary <- pins::pin_read(mount_board(), "aoc") |>
  dplyr::rename(hcpcs            = primary,
                complement       = addon,
                deleted          = primary_deleted,
                edit_type        = type,
                edit_description = type_description) |>
  dplyr::mutate(aoc_type         = "primary",
                addon_deleted    = NULL,
                .after           = hcpcs)

addons <- pins::pin_read(mount_board(), "aoc") |>
  dplyr::rename(hcpcs            = addon,
                complement       = primary,
                deleted          = addon_deleted,
                edit_type        = type,
                edit_description = type_description) |>
  dplyr::mutate(aoc_type         = "addon",
                primary_deleted  = NULL,
                .after           = hcpcs)

aoc_long <- vctrs::vec_rbind(addons, primary) |>
  tidyr::nest(complements = c(complement)) |>
  dplyr::select(
    hcpcs,
    aoc_type,
    complements,
    deleted,
    edit_type,
    edit_description,
    edit_effective,
    edit_deleted,
    notes
  ) |>
  dplyr::arrange(hcpcs, edit_effective)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    aoc_long,
    name = "aoc_long",
    title = "Add-on Code Edits - Long",
    description = "Medicare NCCI Add-on Code Edits 2024-04-01",
    type = "qs"
  )

board |> pins::write_board_manifest()


# Previous function

compare_addons <- function(hcpcs, ...) {

  types <- is_aoc_type(hcpcs)

  x <- list(
    addon   = vctrs::vec_c(types$both, types$addon),
    primary = vctrs::vec_c(types$both, types$primary)
  )

  primary <- pins::pin_read(mount_board(), "aoc") |>
    dplyr::rename(hcpcs         = primary,
                  complement    = addon,
                  deleted       = primary_deleted) |>
    dplyr::mutate(aoc_type      = "primary",
                  addon_deleted = NULL,
                  .after        = hcpcs)

  addons <- pins::pin_read(mount_board(), "aoc") |>
    dplyr::rename(hcpcs           = addon,
                  complement      = primary,
                  deleted         = addon_deleted) |>
    dplyr::mutate(aoc_type        = "addon",
                  primary_deleted = NULL,
                  .after          = hcpcs)

  add_ifelse <- function(x, df, dfcol, by) {
    if (vctrs::vec_is_empty(x)) { NULL } else {
      vctrs::vec_slice(df,
                       vctrs::vec_in(dfcol, x)) |>
        tidyr::nest(.by = {{ by }}) }
  }

  res <- list(
    addon   = add_ifelse(x$addon, addons, addons$hcpcs, hcpcs),
    primary = add_ifelse(x$primary, primary, primary$hcpcs, hcpcs)
  )

  vctrs::vec_rbind(res$addon, res$primary) |>
    tidyr::unnest(data) |>
    tidyr::nest(complements = c(complement)) |>
    dplyr::select(
      hcpcs,
      aoc_type,
      complements,
      deleted,
      type,
      type_description,
      edit_effective,
      edit_deleted,
      notes
    ) |>
    dplyr::arrange(hcpcs, edit_effective)
}
