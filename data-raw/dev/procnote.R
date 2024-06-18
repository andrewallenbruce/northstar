source(here::here("data-raw", "source_setup", "setup.R"))

# proc_note <- readr::read_lines(procnotes)[4:602]
#
# proc_note <- vctrs::vec_c(
#   proc_note[1:359],
#   proc_note[361:599]
# ) |>
#   stringr::str_remove_all("\\t") |>
#   stringr::str_remove_all(stringr::fixed("*")) |>
#   stringr::str_squish() |>
#   dplyr::na_if("")
#
# proc_note <- proc_note[!is.na(proc_note)]
#
# proc_note <- dplyr::tibble(note = proc_note) |>
#   tidyr::separate_wider_delim(
#     cols = note,
#     delim = "--",
#     names = c("note", "desc"),
#     too_few = "align_end"
#   ) |>
#   tidyr::fill(note)
#
# proc_note <- proc_note |>
#   dplyr::mutate(id = dplyr::consecutive_id(note)) |>
#   dplyr::group_by(id) |>
#   tidyr::nest(strings = c(desc)) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(description = purrr::map(strings, ~paste0(., collapse = " "))) |>
#   tidyr::unnest(cols = c(description)) |>
#   dplyr::ungroup() |>
#   dplyr::select(note, description) |>
#   dplyr::mutate(
#     description = stringr::str_replace_all(description, '"', " "),
#     description = stringr::str_squish(description),
#     delete = stringr::str_detect(description, "THIS PROCESSING NOTE DELETED"),
#     date_deleted = dplyr::if_else(delete, stringr::str_extract(
#       description, stringr::regex("[0-9]{1}/[0-9]{1}/[0-9]{2}")), NA_character_),
#     date_deleted = clock::date_parse(date_deleted, format = "%1m/%1d/%y"),
#     delete = NULL)

proc_note <- proc_note |>
  dplyr::select(
    procnote = note,
    procnote_desc = description,
    procnote_deleted = date_deleted
  )

# Join with proc
two <- two |>
  dplyr::left_join(
    proc_note,
    by = dplyr::join_by(procnote)) |>
  dplyr::select(
    hcpcs,
    type,
    two_desc_short,
    two_desc_long,
    date_added,
    date_term,
    price,
    mprice,
    labcert,
    xref,
    coverage,
    tos,
    betos,
    procnote,
    procnote_desc,
    procnote_deleted,
    cim,
    mcm,
    statute,
    asc_group,
    asc_date,
    anesth,
    action_date,
    action
  ) |>
  janitor::remove_empty(which = c("rows", "cols"))
