source(here::here("data-raw", "source_setup", "setup.R"))

# Medicare NCCI Procedure to Procedure (PTP) Edits
# Column 3: * = in existence prior to 1996
# Column 4: Modifier 0 = Not Allowed, 1 = Allowed, 9 = Not Applicable

# df %>% filter(if_all(starts_with("value"), ~ . == first(.)))

# adj_re2 <- search_adjustments() |>
#   select(adj_code, adj_type) |>
#   mutate(adj = map(adj_code, \(x) infer_regex(x))) |>
#   unnest_wider(adj, names_sep = "_")


"https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits"

ptp_zips <- fs::dir_ls("C:/Users/Andrew/Downloads", regexp = "*[f][0-9].zip$")

# Unzip
purrr::walk(as.character(ptp_zips), zip::unzip)

# Delete zips
fs::file_delete(ptp_zips)

xlsx_paths <- fs::file_info(fs::dir_ls(here::here())) |>
  dplyr::pull(path) |>
  as.character() |>
  str_subset("xlsx")

# Delete txts and csvs
fs::file_delete(here::here(fs::dir_ls(glob = "*.csv|*.txt|*.TXT")))

ptp <- xlsx_paths |>
  map(read_excel, col_types = "text") |>
  map(fuimus::df_2_chr) |>
  set_names(
    str_remove_all(
      make_clean_names(
        basename(xlsx_paths)), ".xlsx"))

process_ptp <- function(x, type = "Practitioner") {

  x |>
    janitor::row_to_names(row_number = "find_header") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(column_1)) |>
    dplyr::reframe(
      column_1,
      column_2,
      before_96 = dplyr::if_else(in_existence == "*", TRUE, FALSE),
      effective = anytime::anydate(effective),
      deletion = dplyr::if_else(deletion == "*", "99991231", deletion),
      deletion = anytime::anydate(deletion),
      modifier = as.integer(modifier),
      rationale = ptp_edit_rationale,
      service = type
    )
}

ptp_prac <- ptp[5:8] |>
  map(process_ptp) |>
  list_rbind()

ptp_out <- ptp[1:4] |>
  map(process_ptp, type = "Outpatient") |>
  list_rbind()

ncci_ptp <- vctrs::vec_rbind(ptp_prac, ptp_out)

# Update Pin
pin_update(
  ncci_ptp,
  name = "ncci_ptp",
  title = "Procedure-to-Procedure Edits",
  description = "Medicare NCCI Procedure-to-Procedure Edits 07-01-2024"
)

fs::file_delete(here::here(fs::dir_ls(glob = "*.xlsx")))

comprehensive <- ptp_prac |>
  reframe(
    hcpcs_code = column_1,
    ptp_type = "Comprehensive",
    ptp_complement = column_2,
    ptp_deleted = deletion,
    ptp_mod = modifier,
    ptp_rationale = rationale
         ) |>
  nest(ptp_complements = c(ptp_complement)) |>
  relocate(ptp_complements, .after = ptp_type) |>
  filter(ptp_deleted >= Sys.Date()) |>
  select(-ptp_deleted)

component <- ptp_prac |>
  reframe(
    hcpcs_code = column_2,
    ptp_type = "Component",
    ptp_complement = column_1,
    ptp_deleted = deletion,
    ptp_mod = modifier,
    ptp_rationale = rationale
  ) |>
  nest(ptp_complements = c(ptp_complement)) |>
  relocate(ptp_complements, .after = ptp_type) |>
  filter(ptp_deleted >= Sys.Date()) |>
  select(-ptp_deleted)

ptp_nest <- vctrs::vec_rbind(comprehensive, component)

# Update Pin
pin_update(
  ptp_nest,
  name = "ncci_ptp_nested",
  title = "Procedure-to-Procedure Edits",
  description = "Medicare NCCI Procedure-to-Procedure Edits 07-01-2024"
)

# dplyr::case_when(
#   ptp_edit_mod == 0 ~ "Not Allowed",
#   ptp_edit_mod == 1 ~ "Allowed",
#   ptp_edit_mod == 9 ~ "Not Applicable"
# )
