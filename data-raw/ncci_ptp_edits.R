source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

ptps <- ptp_paths |>
  map(read_excel, col_types = "text") |>
  map(fuimus::df_2_chr)

# Medicare NCCI Procedure to Procedure (PTP) Edits
# Column 3: * = in existence prior to 1996
# Column 4: Modifier 0 = Not Allowed, 1 = Allowed, 9 = Not Applicable
# https://www.cms.gov/medicare/coding-billing/national-correct-coding-initiative-ncci-edits/medicare-ncci-procedure-procedure-ptp-edits
ptp1 <- ptps$`ccipra-v301r0-f1` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
    )


ptp2 <- ptps$`ccipra-v301r0-f2` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp3 <- ptps$`ccipra-v301r0-f3` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp4 <- ptps$`ccipra-v301r0-f4` |>
  row_to_names(row_number = 2) |>
  clean_names() |>
  slice(4:n()) |>
  mutate(deletion  = if_else(deletion == "*", "99991231", deletion),
         effective = anytime::anydate(effective),
         deletion  = anytime::anydate(deletion),
         modifier  = as.integer(modifier)) |>
  select(
    column_1,
    column_2,
    exist96 = in_existence,
    deletion,
    modifier,
    rationale = ptp_edit_rationale
  )

ptp <- vctrs::vec_rbind(ptp1, ptp2, ptp3, ptp4)

comprehensive <- ptp |>
  select(hcpcs              = column_1,
         ptp_complement     = column_2,
         ptp_deleted        = deletion,
         ptp_edit_mod       = modifier,
         ptp_edit_rationale = rationale
         ) |>
  mutate(ptp_type           = "comprehensive",
         .after             = hcpcs
         ) |>
  nest(ptp_complements      = c(ptp_complement))

component <- ptp |>
  select(hcpcs              = column_2,
         ptp_complement     = column_1,
         ptp_deleted        = deletion,
         ptp_edit_mod       = modifier,
         ptp_edit_rationale = rationale
         ) |>
  mutate(ptp_type           = "component",
         .after             = hcpcs
         ) |>
  nest(ptp_complements      = c(ptp_complement))

ptp_long <- vctrs::vec_rbind(comprehensive, component) |>
  dplyr::mutate(
    ptp_edit_mod_desc = dplyr::case_when(
      ptp_edit_mod == 0 ~ "Not Allowed",
      ptp_edit_mod == 1 ~ "Allowed",
      ptp_edit_mod == 9 ~ "Not Applicable"
      ),
    .after = ptp_edit_mod
    ) |>
  dplyr::select(
    hcpcs,
    ptp_type,
    ptp_complements,
    ptp_deleted,
    ptp_edit_mod,
    ptp_edit_mod_desc,
    ptp_edit_rationale
  ) |>
  dplyr::arrange(hcpcs, ptp_deleted)

# Update Pin
pin_update(
  ptp_long,
  name = "ptp_long",
  title = "Procedure-to-Procedure Edits - Long",
  description = "Medicare NCCI Procedure-to-Procedure Edits 2024-04-01"
)

list_pins()
