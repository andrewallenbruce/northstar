#--------------------------- EM Codes
library(unglue)

`%chin%` <- data.table::`%chin%`

em <- descriptors() |>
  filter(cpt %chin% c(as.character(99202:99215))) |>
  select(cpt, clinician_descriptor) |>
  unite("code", cpt:clinician_descriptor, sep = ": ") |>
  # as.data.frame()
  pull(code)


library(inferregex)
infer_regex(c("I knew it!"))


pt1 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient with {mdm}")
pt2 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient, including medically appropriate {procedure} and {mdm} medical decision making, total time {begin}-{end} minutes")
pt3 <- c("{cpt}: Outpatient visit for evaluation and management of {type} patient, including medically appropriate {procedure} and {mdm} medical decision making, total time {begin}-{end}")

unglue_data(em, c(pt1, pt2, pt3)) |>
  dplyr::tibble() |>
  # unite("minutes", begin:end, sep = "-", na.rm = TRUE) |>
  mutate(
    # minutes = na_if(minutes, ""),
    type = stringr::str_to_title(type),
    procedure = stringr::str_to_title(procedure),
    mdm = stringr::str_remove(mdm, " level of"),
    mdm = stringr::str_to_title(mdm)) |>
  select(cpt,
         patient = type,
         procedure,
         medical_decision_making = mdm,
         from = begin,
         to = end) |>
  mutate(from = as.integer(from),
         to = as.integer(to),
         minutes = ivs::iv(from, to)) |>
  print(n = Inf)
