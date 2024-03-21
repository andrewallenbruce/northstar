base <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/"
x <- rvest::session(base) |>
  rvest::session_follow_link("2024") |>
  rvest::html_elements("a") |>
  rvest::html_attr("href")

x[2:8]
