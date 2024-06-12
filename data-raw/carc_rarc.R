source(here::here("data-raw", "source_setup", "setup.R"))

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

adj_group <- dplyr::tibble(
  code = c("CO", "CR", "OA", "PI", "PR"),
  description = c("Contractual Obligations",
                  "Corrections and Reversals",
                  "Other Adjustments",
                  "Payer Initiated Reductions",
                  "Patient Responsibility"),
)

# Update Pin
pin_update(
  adj_group,
  name        = "adj_group",
  title       = "CARC Group Codes",
  description = "Claim Adjustment Reason Group Codes"
)

adj_carc <- read_html("https://x12.org/codes/claim-adjustment-reason-codes") |>
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
  select(
    code,
    description,
    usage,
    notes,
    start_date,
    last_modified,
    end_date
    )


pin_update(
  adj_carc,
  name        = "adj_carc",
  title       = "CARC Codes",
  description = "Claim Adjustment Reason Codes"
)

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

adj_rarc <- read_html("https://x12.org/codes/remittance-advice-remark-codes") |>
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
  select(
    code,
    description,
    notes,
    start_date,
    last_modified
    )

# Update Pin
pin_update(
  adj_rarc,
  name        = "adj_rarc",
  title       = "RARC Codes",
  description = "Remittance Advice Remark Codes"
)

library(triebeard)

adj_trie <- triebeard::trie(
  keys = c(
    adj_group$code,
    adj_carc$code,
    adj_rarc$code),
  values = c(
    adj_group$description,
    adj_carc$description,
    adj_rarc$description)
)

adj_group$code |> deframe() |> unlist()

# Will not Work!
# pin_update(
#   adj_trie,
#   name        = "adj_trie",
#   title       = "CARCs & RARCs",
#   description = "Triebeard Object containing CARC and RARC Codes and Descriptions"
# )
