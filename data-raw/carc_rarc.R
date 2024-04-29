source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))

# Claim Adjustment Reason Codes (CARCs)
# X12 External Code Source 139
#
# These codes describe why a claim or service line was paid differently than it was billed.
#
# Did you receive a code from a health plan, such as: PR32 or CO286?
# The "PR" is a Claim Adjustment Group Code and the description for "32" is below.
# The Claim Adjustment Group Codes are internal to the X12 standard.
# These codes generally assign responsibility for the adjustment amounts.
# The format is always two alpha characters.
# For convenience, the values and definitions are below:

carc_group_codes <- dplyr::tibble(
  code = c("CO", "CR", "OA", "PI", "PR"),
  description = c("Contractual Obligations", "Corrections and Reversals", "Other Adjustments", "Payer Initiated Reductions", "Patient Responsibility"),
)

carc_codes <- read_html("https://x12.org/codes/claim-adjustment-reason-codes") |>
  html_element(".code_list__code-list-table") |>
  html_table() |>
  select(code = X1, X2) |>
  mutate(description   = strex::str_before_first(X2, "Start: "),
         dates         = strex::str_after_first(X2, "Start: "),
         X2            = NULL,
         start_date    = strex::str_before_first(dates, " "),
         start_date    = case_when(is.na(start_date) ~ dates, TRUE ~ start_date),
         last_modified = strex::str_after_first(dates, "Last Modified: "),
         notes         = strex::str_after_first(dates, "Notes: "),
         end_date      = strex::str_after_first(dates, "Stop: "),
         start_date    = anytime::anydate(start_date),
         end_date      = anytime::anydate(end_date),
         last_modified = anytime::anydate(last_modified),
         usage         = strex::str_after_first(description, "Usage: "),
         description   = case_when(!is.na(usage) ~ strex::str_before_first(description, "Usage: "), TRUE ~ description),
         description   = stringr::str_squish(description)
  ) |>
  select(code, description, usage, notes, start_date, last_modified, end_date)


# Remittance Advice Remark Codes (RARCs)
# X12 External Code Source 411
#
# These codes provide additional explanation for an adjustment already
# described by a Claim Adjustment Reason Code (CARC) or convey information
# about remittance processing.
#
# Remittance Advice Remark Codes (RARCs) are used to provide additional
# explanation for an adjustment already described by a Claim Adjustment Reason
# Code (CARC) or to convey information about remittance processing. Each RARC
# identifies a specific message as shown in the Remittance Advice Remark
# Code List. There are two types of RARCs, supplemental and informational.
# The majority of the RARCs are supplemental; these are generally referred to
# as RARCs without further distinction. Supplemental RARCs provide additional
# explanation for an adjustment already described by a CARC. The second type of
# RARC is informational; these RARCs are all prefaced with Alert: and are
# often referred to as Alerts. Alerts are used to convey information about
# remittance processing and are never related to a specific adjustment or CARC.

rarc_codes <- read_html("https://x12.org/codes/remittance-advice-remark-codes") |>
  html_element(".code_list__code-list-table") |>
  html_table() |>
  select(code = X1, X2) |>
  mutate(description   = strex::str_before_first(X2, "Start: "),
         dates         = strex::str_after_first(X2, "Start: "),
         X2            = NULL,
         start_date    = strex::str_before_first(dates, " "),
         start_date    = case_when(is.na(start_date) ~ dates, TRUE ~ start_date),
         last_modified = strex::str_after_first(dates, "Last Modified: "),
         notes         = strex::str_after_first(dates, "Notes: "),
         start_date    = anytime::anydate(start_date),
         last_modified = anytime::anydate(last_modified),
         description   = stringr::str_squish(description)) |>
  select(code, description, notes, start_date, last_modified)



rarc_carc <- list(
  group = carc_group_codes,
  carc = carc_codes,
  rarc = rarc_codes
  )

# Update Pin
pin_update(
  rarc_carc,
  name        = "rarc_carc",
  title       = "CARCs & RARCs",
  description = "Claim Adjustment Reason Codes (CARCs) and Remittance Advice Remark Codes (RARCs)"
)
