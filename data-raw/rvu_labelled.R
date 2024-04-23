library(sjlabelled)
library(datawizard)
library(rlang)

#------------------ Status Codes
list_stat <- dplyr::tibble(status_code = sort(c(
  "A", "B", "C", "D", "E", "F", "I", "M", "R", "N", "J", "P", "T", "X"
)),
code = sort(c(
  "A", "B", "C", "D", "E", "F", "I", "M", "R", "N", "J", "P", "T", "X"
))) |>
  case_status(code, desc = FALSE) |>
  filter(status_code != "D") |>
  filter(status_code != "F") |>
  tibble::deframe()


rvu <- assign_labels(
  rvu,
  select = "status",
  variable = "Status",
  values = c(list_stat)
)

#------------------ Modifier 26/TC/53
list_mod <- dplyr::tibble(mod = c("26", "TC", "53")) |>
  case_modifier(mod) |>
  select(-mod_description) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "mod",
  variable = "Modifier",
  values = c(list_mod)
)

#------------------ Global Days
list_glob <- dplyr::tibble(
  global = c("000", "010", "090", "MMM", "XXX", "YYY", "ZZZ"),
  description = c(
    "Endoscopic/Minor Procedure. Preop & Postop RVUs Only on Day of Procedure Included in Payment. E/M Day of Procedure Not Payable.",
    "Minor Procedure. Preop RVUs on Day of Procedure & Postop RVUs During 10-Day Postop Period Included in Payment. E/M Day of Procedure & During 10-Day Postop Period Not Payable.",
    "Major Surgery with 1-Day Preop Period & 90-Day Postop Period included in Payment.",
    "Maternity Code. Usual Global Period Does Not Apply.",
    "Concept Does Not Apply.",
    "Carrier Determines if Global Concept Applies, Establishes Postop Period.",
    "Code Related to Another Service, Always Included in Global Period of Other Service."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "global",
  variable = "Global Days",
  values = c(list_glob)
)

#------------------ PCTC
list_pctc <- dplyr::tibble(code = as.character(0:9)) |>
  case_pctc(code) |>
  mutate(pctc_label = case_match(code, "9" ~ "Concept does not apply.", .default = pctc_label)) |>
  select(-pctc_description) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "pctc",
  variable = "PCTC",
  values = c(list_pctc)
)

#------------------ Multiple Procedure
list_mult <- dplyr::tibble(
  code = as.character(c(0:7, 9)),
  description = c(
    "If Billed Same Day as Another Procedure, Payment is Lower of Actual Charge or Fee Schedule Amount.",
    "If Billed Same Day as Procedure with Indicators 1-3, Rank by Fee Schedule & Apply Reduction [100%-50%-25%-25%-25%]. Payment is Lower of Actual Charge or Reduced Fee Schedule Amount.",
    "If Billed Same Day as Procedure with Indicators 1-3, Rank by Fee Schedule & Apply Reduction [100%-50%-50%-50%-50%]. Payment is Lower of Actual Charge or Reduced Fee Schedule Amount.",
    "If Billed with Endoscopy in Same Family, Apply Multiple Endoscopy Rules to Family before Ranking with Other Same Day Procedures. If Billed with Base Only, Base Not Paid Separately.",
    "Diagnostic Imaging TC: If Billed in Same Session on Same Day as Another in Same Family, Rank by TC Fee Schedule and Pay 100% for Highest Price, 50% for Subsequent Procedures. Subsequent Procedures Payment is Lower of Actual Charge or Reduced Fee Schedule Amount. Subject to 50% TC/5% PC Reduction.",
    "Therapy Service Subject to 50% of Practice Expense Component.",
    "Diagnostic Cardiovascular Service Subject to 25% TC Reduction of Second Highest and Subsequent Procedures.",
    "Diagnostic Ophthalmology Service Subject to 20% TC Reduction of Second Highest and Subsequent Procedures.",
    "Concept Does Not Apply."
  )
) |>
  filter(code != "1") |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "mult_proc",
  variable = "Multiple Procedures",
  values = c(list_mult)
)

#------------------ Diagnostic Imaging Family
list_img <- dplyr::tibble(
  code = as.character(c(88, 99)),
  description = c(
    "Subject to Diagnostic Imaging TC or PC Reduction",
    "Concept Does Not Apply."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "dximg",
  variable = "Diagnostic Imaging Family",
  values = c(list_img)
)

#------------------ Surgical Bilateral
list_bilat <- dplyr::tibble(
  code = as.character(c(0:3, 9)),
  description = c(
    "If Billed with Mod 50 or Mods RT & LT, Payment is Lower of Total Charge for Both Sides or 100% of Fee Schedule for A Single Code.",
    "If Billed with Bilateral Modifier or Twice on Same Day by Any Other Means, Payment is Lower of Total Charge for Both Sides or 150% of Fee Schedule for A Single Code. If Billed as Bilateral Procedure with Other Procedure on Same Day, Apply Bilateral Adjustment Before Multiple Procedure Rules.",
    "If Billed with Mod 50 or Twice on Same Day by Any Other Means, Payment is Lower of Total Charge for Both Sides or 100% of Fee Schedule for A Single Code.",
    "If Billed with Mod 50 or for Both Sides on Same Day by Any Other Means, Payment for Each Side, Organ or Paired Organ Site is Lower of Charge for Each Side or 100% of Fee Schedule for Each Side. If Billed as Bilateral Procedure with Other Procedure on Same Day, Determine Fee Schedule Amount Before Applying Multiple Procedure Rules.",
    "Concept Does Not Apply."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "surg_bilat",
  variable = "Bilateral Surgery",
  values = c(list_bilat)
)

#------------------ Assistant at Surgery
list_asst <- dplyr::tibble(
  code = as.character(c(0:2, 9)),
  description = c(
    "No Payment Restriction if Medical Necessity Documentation Submitted.",
    "Payment Restriction: Assistant cannot be paid.",
    "No Payment Restriction: Assistant can be paid.",
    "Concept Does Not Apply."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "surg_asst",
  variable = "Assistant at Surgery",
  values = c(list_asst)
)

#------------------ Co-Surgeon
list_co <- dplyr::tibble(
  code = as.character(c(0:2, 9)),
  description = c(
    "Not Permitted.",
    "Medical Necessity Documentation Required.",
    "Permitted.",
    "Concept Does Not Apply."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(rvu,
                     select = "surg_co",
                     variable = "Co-Surgeons",
                     values = c(list_co))

#------------------ Team Surgeon
list_team <- dplyr::tibble(
  code = as.character(c(0:2, 9)),
  description = c(
    "Not Permitted.",
    "Medical Necessity Documentation Required.",
    "Permitted.",
    "Concept Does Not Apply."
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "surg_team",
  variable = "Team Surgery",
  values = c(list_team)
)

#------------------ Physician Supervision
list_phys <- dplyr::tibble(
  code = c("01", "02", "03", "04", "05", "09", "21", "66", "6A", "7A"),
  description = c(
    "General Supervision",
    "Direct Supervision",
    "Personal Supervision",
    "General Supervision Unless by Qualified Independent or Clinical Psychologist",
    "General Supervision Unless by Qualified Audiologist.",
    "Concept Does Not Apply.",
    "Direct Supervision unless by Certified Technician under General Supervision",
    "Physician or ABPTS Certified Physical Therapist with Certification in Procedure",
    "Physician or ABPTS Certified Physical Therapist with Certification in Procedure. ABPTS PT May Supervise Another, Only ABPTS PT May Bill",
    "ABPTS Certified Physical Therapist, Uncertified Physical Therapist under Direct Supervision, or Certified Technician under General Supervision. ABPTS PT May Supervise Another, Only ABPTS PT May Bill"
  )
) |>
  tibble::deframe()

rvu <- assign_labels(
  rvu,
  select = "supvis",
  variable = "Physician Supervision of Diagnostic Procedures",
  values = c(list_phys)
)

#------------------ Column Labels
attr(rvu$hcpcs, "label")       <- "HCPCS Code"
attr(rvu$description, "label") <- "HCPCS Description"
attr(rvu$wrvu, "label")        <- "Work RVU"
attr(rvu$nfprvu, "label")      <- "Non-Facility Practice Expense RVU"
attr(rvu$fprvu, "label")       <- "Facility Practice Expense RVU"
attr(rvu$mrvu, "label")        <- "Malpractice RVU"
attr(rvu$ntotal, "label")      <- "Non-Facility Total RVUs"
attr(rvu$ftotal, "label")      <- "Facility Total RVUs"
attr(rvu$cf, "label")          <- "Conversion Factor"
attr(rvu$op_ind, "label")      <- "Operative Indicator"
attr(rvu$op_pre, "label")      <- "Preoperative Percentage"
attr(rvu$op_intra, "label")    <- "Intraoperative Percentage"
attr(rvu$op_post, "label")     <- "Postoperative Percentage"
attr(rvu$nfprvu_opps, "label") <- "Non-Facility Practice Expense OPPS"
attr(rvu$fprvu_opps, "label")  <- "Facility Practice Expense OPPS"
attr(rvu$mrvu_opps, "label")   <- "Malpractice OPPS"
attr(rvu$global, "label")      <- "Global Surgery"
attr(rvu$pctc, "label")        <- "PCTC Indicator"
attr(rvu$endo, "label")        <- "Endoscopic Base Code"
