source(here::here("data-raw", "source_setup", "setup.R"))

forager:::get_pin("denials_extract") |>
  select(contains("denial"))

northstar::search_adjustments()$carc |>
  filter(code == "29")


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
