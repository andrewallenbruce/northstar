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
    # ,
    # adj_code = str_c("-", adj_code)
    ) |>
  distinct() |>
  select(
    adj_code,
    denial_type,
    denial_category = denial_rollup,
    denial_cause = denial_root_cause,
    denial_blame = denial_root_cause_department
    )

# Update Pin
pin_update(
  denials_extract,
  name        = "denials_extract",
  title       = "Common Denials Categories and Types",
  description = "Common Denials Categories and Types"
)

northstar::search_adjustments("rarc") |>
  filter(code == "M51")

northstar::search_adjustments("carc") |>
  filter(code == "B13")


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
  separate_longer_delim(rarc, delim = " ")

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
    denial_reason = pg |>
      rvest::html_elements(".title") |>
      rvest::html_text2(preserve_nbsp = TRUE) |>
      stringr::str_subset("Browse by Topic", negate = TRUE),
    tbl = pg |>
      rvest::html_elements(".table") |>
      rvest::html_table() |>
      purrr::pluck(1)
    # |>
    #   janitor::clean_names() |>
    #   dplyr::select(
    #     adj_code = carc_rarc,
    #     adj_description = description)
    ,
    info = pg |>
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
          "Common Reasons for Message|Next Step|Claim Submission Tips"
        ), .before = text
      ) |>
      tidyr::fill(heading) |>
      dplyr::filter(
        stringr::str_detect(
          text,
          "Common Reasons for Message|Next Step|Claim Submission Tips",
          negate = TRUE
      )
    )
  )

  dplyr::tibble(
    denial_reason = x$denial_reason,
    codes_desc = list(x$tbl),
    instructions = list(x$info)
  )
}


denial_reasons_pages <- purrr::map(
  vec_urls,
  scrape_urls
  ) |>
  purrr::list_rbind()


denials <- list(
  common = denials_common,
  reasons = denial_reasons_pages
)

# Update Pin
pin_update(
  denials,
  name        = "denials_site",
  title       = "Noridian: Most Common Claim Submission Errors",
  description = "Noridian: Most Common Claim Submission Errors"
)
