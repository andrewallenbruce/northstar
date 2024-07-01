# "CodeRight: CPT E/M Coding Assistant"
#
# "E/M Code Calculator"
#
# "Patient Type:"
#
# "New Patient"
# "Established Patient"
#
# "99211: Outpatient visit for evaluation and management of established patient with minimal presenting problem"
#
# "History Level:"
# "Problem Focused"
# "Expanded Problem Focused"
# "Detailed"
# "Comprehensive"
#
# "Examination Level:"
# "Problem Focused"
# "Expanded Problem Focused"
# "Detailed"
# "Comprehensive"
#
# "Medical Decision Making Level:"
# "Straightforward"
# "Low Complexity"
# "Moderate Complexity"
# "High Complexity"
#
# "Total Time Spent (minutes):"

em_code_calculator <- function(patient = c("new", "established"),
                               # history = c("problem", "expanded_problem", "detailed", "comprehensive"),
                               # exam = c("problem", "expanded_problem", "detailed", "comprehensive"),
                               mdm = c("straightforward", "low", "moderate", "high"),
                               minutes) {

  min_msg <- "`minutes` must be a numeric value between 10 and 74."

  # stopifnot(min_msg == is.numeric(minutes) && dplyr::between(minutes, 10, 74))

  patient <- match.arg(patient)
  mdm <- match.arg(mdm)

  if (patient == "new") {
    cpt <- dplyr::case_when(
      # High complexity MDM or 60-74 minutes total time
      mdm == "high"            | dplyr::between(minutes, 60, 74) ~ "99205",
      # Moderate complexity MDM or 45-59 minutes total time
      mdm == "moderate"        | dplyr::between(minutes, 45, 59) ~ "99204",
      # Low complexity MDM or 30-44 minutes total time
      mdm == "low"             | dplyr::between(minutes, 30, 44) ~ "99203",
      # Straightforward MDM or 15-29 minutes total time
      mdm == "straightforward" | dplyr::between(minutes, 15, 29) ~ "99202"
    )
  }

  if (patient == "established") {
    cpt <- dplyr::case_when(
      # High complexity MDM or 40-54 minutes total time
      mdm == "high"            | dplyr::between(minutes, 40, 54) ~ "99215",
      # Moderate complexity MDM or 30-39 minutes total time
      mdm == "moderate"        | dplyr::between(minutes, 30, 39) ~ "99214",
      # Low complexity MDM or 20-29 minutes total time
      mdm == "low"             | dplyr::between(minutes, 20, 29) ~ "99213",
      # Straightforward MDM or 10-19 minutes total time
      mdm == "straightforward" | dplyr::between(minutes, 10, 19) ~ "99212",
      .default = "99211"
    )
  }

  explanation <- list(
    "99205" = "High complexity MDM or 60-74 minutes total time",
    "99204" = "Moderate complexity MDM or 45-59 minutes total time",
    "99203" = "Low complexity MDM or 30-44 minutes total time",
    "99202" = "Straightforward MDM or 15-29 minutes total time"
  )

  cat(glue::glue("Suggested CPT Code: {cpt}\nExplanation: {explanation[[cpt]]}"))

  invisible(cpt)
}

em_code_calculator(patient = "new",
                   mdm = "high",
                   minutes = 60)

# if (patientType === 'new') {
#   if (mdmLevel === 'high' || timeSpent >= 60) {
#     cptCode = '99205';
#     explanation = 'High complexity MDM or 60-74 minutes total time';
#   } else if (mdmLevel === 'moderate' || timeSpent >= 45) {
#     cptCode = '99204';
#     explanation = 'Moderate complexity MDM or 45-59 minutes total time';
#   } else if (mdmLevel === 'low' || timeSpent >= 30) {
#     cptCode = '99203';
#     explanation = 'Low complexity MDM or 30-44 minutes total time';
#   } else {
#     cptCode = '99202';
#     explanation = 'Straightforward MDM or 15-29 minutes total time';
#   }
# } else {
#   if (mdmLevel === 'high' || timeSpent >= 40) {
#     cptCode = '99215';
#     explanation = 'High complexity MDM or 40-54 minutes total time';
#   } else if (mdmLevel === 'moderate' || timeSpent >= 30) {
#     cptCode = '99214';
#     explanation = 'Moderate complexity MDM or 30-39 minutes total time';
#   } else if (mdmLevel === 'low' || timeSpent >= 20) {
#     cptCode = '99213';
#     explanation = 'Low complexity MDM or 20-29 minutes total time';
#   } else {
#     cptCode = '99212';
#     explanation = 'Straightforward MDM or 10-19 minutes total time';
#   }
# }


#--------------------------- EM Codes
library(unglue)

`%chin%` <- data.table::`%chin%`

# search_descriptions(
#   hcpcs_code = c(as.character(99202:99215)),
#   hcpcs_desc_type = "Clinician"
#   ) |>
clin <- codexchain::clinician |>
  select(
    hcpcs_code = cpt_code,
    hcpcs_description = clinician_descriptor) |>
  filter(hcpcs_code %chin% c("99202", "99203", "99204", "99205", "99211", "99212", "99213", "99214", "99215"))

clin |>
  # mutate(hcpcs_description = strex::str_after_first(hcpcs_description, ", including medically appropriate "), .after = hcpcs_code) |>
  print(n = Inf)


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
