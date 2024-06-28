source(here::here("data-raw", "source_setup", "setup.R"))

# Claim Adjustment Reason Codes (CARCs)
# communicate an adjustment, meaning that
# they must communicate why a claim or
# service line was paid differently than
# it was billed. If there is no adjustment
# to a claim/line, then there is no
# adjustment reason code.
#
# Remittance Advice Remark Codes (RARCs) are
# used to provide additional explanation for
# an adjustment already described by a CARC
# or to convey information about remittance
# processing


denials_extract <- forager:::get_pin("denials_extract") |>
  select(
    contains("denial"),
    -impactable_denial_flag,
    -denial_date,
    -denial_amount
    ) |>
  separate_wider_delim(
    denial_code,
    delim = " - ",
    names = c("adj_code", "adj_desc"),
    too_few = "align_start") |>
  mutate(
    adj_desc = NULL,
    adj_code = str_remove_all(adj_code, " -")
    ) |>
  distinct() |>
  reframe(
    adj_code,
    denial_type,
    denial_category = denial_rollup,
    denial_cause = denial_root_cause,
    denial_category = case_when(
      denial_category == "Billing/Claim Error" ~ "Billing Error: Invalid Code",
      TRUE ~ denial_category),
    denial_cause = case_when(
      denial_category == denial_cause ~ NA_character_,
      denial_category == "Medical Necessity" ~ NA_character_,
      denial_category == "Billing Error: Invalid Code" ~ NA_character_,
      denial_cause == "Service(s) Not Covered" ~ NA_character_,
      TRUE ~ denial_cause
    ),
    ) |>
  select(-denial_cause)


# Update Pin
pin_update(
  denials_extract,
  name        = "denials_extract",
  title       = "Common Denials Categories and Types",
  description = "Common Denials Categories and Types"
)

northstar::search_adjustments() |>
  filter(adj_type == "RARC")

northstar::search_adjustments() |>
  filter(adj_code == "MA121")


denials_common <- read_html(
  "https://med.noridianmedicare.com/web/jeb/topics/claim-submission/denial-resolution") |>
  html_element(xpath = '//*[(@id = "tableToSearch")]') |>
  html_table() |>
  clean_names() |>
  select(
    carc = reason_code,
    rarc = remark_code_s,
    denial_reason = denial,
    denial_description
    ) |>
  mutate(rarc = na_if(rarc, "")) |>
  separate_longer_delim(carc, delim = " | ") |>
  separate_longer_delim(rarc, delim = " | ") |>
  separate_longer_delim(rarc, delim = " ") |>
  reframe(
    adj_CARC = carc,
    adj_RARC = rarc,
    denial_reason
  )

denials_common

partial_urls <- read_html(
  "https://med.noridianmedicare.com/web/jeb/topics/claim-submission/denial-resolution") |>
  html_element(xpath = '//*[(@id = "tableToSearch")]') |>
  html_elements("a") |>
  html_attr("href") |>
  unique() |>
  str_subset("https:", negate = TRUE)

vec_urls <- glue::glue("https://med.noridianmedicare.com{partial_urls}")

scrape_urls <- function(url) {

  pg <- rvest::read_html(url)

  x <- list(
    denial_reason = fuimus::null_if_empty(
      pg |>
      rvest::html_elements(".title") |>
      rvest::html_text2(preserve_nbsp = TRUE) |>
      stringr::str_subset("Browse by Topic", negate = TRUE)
      ),
    tbl = fuimus::null_if_empty(
      pg |>
      rvest::html_elements(".table") |>
      rvest::html_table() |>
      purrr::pluck(1)
      ),
    info = fuimus::null_if_empty(
      pg |>
      rvest::html_elements(".portlet-journal-content .journal-content-article") |>
      rvest::html_elements("h3, ul") |>
      rvest::html_text2(preserve_nbsp = FALSE) |>
      tibble::enframe(
        name = "row",
        value = "text"
      ) |>
      dplyr::mutate(row = NULL) |>
      tidyr::separate_longer_delim(
        text,
        delim = "\n"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        heading = stringr::str_extract(
          text,
          "Common Reasons for Message|Common Reason for Message|Next Step|Claim Submission Tips"
        ), .before = text
      ) # |>
      # tidyr::fill(heading) |>
      # dplyr::filter(
      #   stringr::str_detect(
      #     text,
      #     "Common Reasons for Message|Common Reason for Message|Next Step|Claim Submission Tips",
      #     negate = TRUE
     # )
    # )
   )
  )

 dplyr::tibble(
    denial_reason = x$denial_reason,
    codes_desc = list(x$tbl),
    instructions = list(x$info)
  )
}

denial_pages <- purrr::map(
  vec_urls,
  scrape_urls
  ) |>
  purrr::list_rbind()

denials_common

denial_reasons <- denial_pages |>
  select(denial_reason, codes_desc) |>
  unnest(codes_desc) |>
  clean_names() |>
  mutate(description = if_else(is.na(description), description_2, description),
         description_2 = NULL) |>
  separate_longer_delim(carc_rarc, delim = " ") |>
  rename(adj_code = carc_rarc) |>
  select(adj_code, denial_reason)

next_step_tips <- denial_pages |>
  select(denial_reason, instructions) |>
  unnest(instructions) |>
  fill(heading) |>
  mutate(heading = if_else(heading == "Common Reason for Message", "Common Reasons for Message", heading)) |>
  pivot_wider(
    names_from = heading,
    values_from = text,
    values_fn = list) |>
  clean_names() |>

  unnest(common_reasons_for_message) |>
  filter(common_reasons_for_message != "Common Reasons for Message") |>
  nest(common_reasons = c(common_reasons_for_message)) |>
  rowwise() |>
  mutate(common_reasons = map_chr(common_reasons, ~paste0(., collapse = ". "))) |>
  unnest(common_reasons) |>
  ungroup() |>

  unnest(next_step) |>
  filter(next_step != "Next Step") |>
  nest(next_steps = c(next_step)) |>
  rowwise() |>
  mutate(next_steps = map_chr(next_steps, ~paste0(., collapse = ". "))) |>
  unnest(next_steps) |>
  ungroup() |>
  mutate(next_steps = str_remove_all(next_steps, '"')) |>

  unnest(claim_submission_tips) |>
  filter(claim_submission_tips != "Claim Submission Tips") |>
  nest(claim_submission_tips = c(claim_submission_tips)) |>
  rowwise() |>
  mutate(claim_submission_tips = map_chr(claim_submission_tips, ~paste0(., collapse = ". "))) |>
  unnest(claim_submission_tips) |>
  ungroup() |>
  mutate(claim_submission_tips = str_remove_all(claim_submission_tips, '"'))

denials <- denial_reasons |>
  left_join(
    next_step_tips,
    by = join_by(denial_reason))

# Update Pin
pin_update(
  denials,
  name        = "denials_site",
  title       = "Noridian: Most Common Claim Submission Errors",
  description = "Noridian: Most Common Claim Submission Errors"
)
