source(here::here("data-raw", "source_setup", "setup.R"))

"rvu_descriptions"
"cpt_descriptions"
"two_descriptions"
"hcpcs_noc"

hcpcs_desc <- get_pin("hcpcs_desc_raw") |>
  dplyr::count(hcpcs_code, hcpcs_desc_type, sort = TRUE) |>
  dplyr::filter(hcpcs_desc_type == "Clinician") |>
  dplyr::distinct(
    hcpcs_code,
    hcpcs_description,
    .keep_all = TRUE
    )

# Add Levels and Categories ---------------------
hcpcs_desc <- hcpcs_desc |>
  dplyr::mutate(
    hcpcs_level = dplyr::case_when(
      stringr::str_detect(
        hcpcs_code,
        stringr::regex("^\\d{4}[A-Z0-9]$")) ~ "I",
      stringr::str_detect(
        hcpcs_code,
        stringr::regex("^[A-V]\\d{4}$")) ~ "II",
    .default = NA_character_
  ) |> forcats::as_factor(),
  hcpcs_category = dplyr::case_when(
    hcpcs_level == "I" & stringr::str_ends(
      hcpcs_code, stringr::regex("[A-EG-SU-Z0-9]")) ~ "I",
    hcpcs_level == "I" & stringr::str_ends(
      hcpcs_code, stringr::regex("[F]")) ~ "II",
    hcpcs_level == "I" & stringr::str_ends(
      hcpcs_code, stringr::regex("[T]")) ~ "III",
    .default = NA_character_
  ) |> forcats::as_factor(),
  .after = hcpcs_code,
  hcpcs_desc_type = forcats::as_factor(hcpcs_desc_type),
  hcpcs_desc_type = forcats::fct_infreq(hcpcs_desc_type, ordered = TRUE)
  ) |>
  dplyr::arrange(hcpcs_code, hcpcs_desc_type)


# Remove 'Short' Duplicates ---------------------
hcpcs_desc <- hcpcs_desc |>
  dplyr::mutate(
    rowid = dplyr::row_number(),
    .before = hcpcs_code
    )

vec_short_dupes <- hcpcs_desc |>
  dplyr::filter(hcpcs_desc_type == "Short") |>
  dplyr::count(hcpcs_code, sort = TRUE) |>
  dplyr::filter(n > 1) |>
  dplyr::pull(hcpcs_code)

dupes_rowid <- hcpcs_desc |>
  dplyr::filter(
    hcpcs_desc_type == "Short",
    hcpcs_code %in% vec_short_dupes
    ) |>
  dplyr::slice_max(rowid, by = hcpcs_code) |>
  dplyr::pull(rowid)

hcpcs_desc <- hcpcs_desc |>
  dplyr::filter(!rowid %in% dupes_rowid) |>
  dplyr::mutate(
    hcpcs_description = dplyr::case_when(
      hcpcs_desc_type == "Short" ~ stringr::str_to_upper(hcpcs_description),
      .default = hcpcs_description
    ),
    rowid = NULL
  )

# Add Sections and Ranges ---------------------
hcpcs_desc <- hcpcs_desc |>
  dplyr::mutate(
    hcpcs_section = dplyr::case_match(
    hcpcs_code,

    # Level I Codes
    as.character(99202:99499) ~ "Evaluation and Management",
    c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140) ~ "Anesthesiology",
    as.character(10004:69990) ~ "Surgery",
    as.character(70010:79999) ~ "Radiology",
    as.character(80047:89398) ~ "Pathology and Laboratory",
    as.character(c(90281:99199, 99500:99607)) ~ "Medicine",

    ## Category I Immunization Codes (ends in A)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "A") ~ "Immunization",

    ## Category I Administrative MAAA Codes
    ## (Multianalyte Assays With Algorithmic Analyses Codes) (ends in M)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "M") ~ "Administrative Multianalyte Assay With Algorithmic Analysis",

    ## Category I Proprietary Laboratory Analyses (PLA) Codes (ends in U)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "U") ~ "Proprietary Laboratory Analysis",

    ## Category II Performance Measurement Codes (ends in F)
    paste0(stringr::str_pad(1:15, width = 4, pad = "0"), "F") ~ "Composite Measures",
    paste0(stringr::str_pad(500:584, width = 4, pad = "0"), "F") ~ "Patient Management",
    paste0(stringr::str_pad(1000:1505, width = 4, pad = "0"), "F") ~ "Patient History",
    paste0(stringr::str_pad(2000:2060, width = 4, pad = "0"), "F") ~ "Physical Examination",
    paste0(stringr::str_pad(3006:3776, width = 4, pad = "0"), "F") ~ "Diagnostic/Screening Processes or Results",
    paste0(stringr::str_pad(4000:4563, width = 4, pad = "0"), "F") ~ "Therapeutic, Preventive or Other Interventions",
    paste0(stringr::str_pad(5005:5250, width = 4, pad = "0"), "F") ~ "Follow-Up or Other Outcomes",
    paste0(stringr::str_pad(6005:6150, width = 4, pad = "0"), "F") ~ "Patient Safety",
    paste0(stringr::str_pad(7010:7025, width = 4, pad = "0"), "F") ~ "Structural Measures",
    paste0(stringr::str_pad(9001:9007, width = 4, pad = "0"), "F") ~ "Non-Measure Claims Based Reporting",

    ## Category III Temporary Codes (ends in T)
    paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "T") ~ "Temporary Codes",

    # Level II Codes

    paste0("A", stringr::str_pad(21:999, width = 4, pad = "0")) ~ "Transportation Services Including Ambulance",
    paste0("A", stringr::str_pad(2000:9999, width = 4, pad = "0")) ~ "Medical and Surgical Supplies",
    paste0("B", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Enteral and Parenteral Therapy",
    paste0("C", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Outpatient PPS",
    paste0("D", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Dental Codes",
    paste0("E", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Durable Medical Equipment",
    paste0("G", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Procedures/Professional Services (Temporary)",
    paste0("H", stringr::str_pad(1:2037, width = 4, pad = "0")) ~ "Alcohol and Drug Abuse Treatment Services",
    paste0("H", stringr::str_pad(2038:2041, width = 4, pad = "0")) ~ "Rehabilitative Services",
    paste0("J", stringr::str_pad(120:8499, width = 4, pad = "0")) ~ "Drugs Administered Other Than Oral Method",
    paste0("J", stringr::str_pad(8501:9999, width = 4, pad = "0")) ~ "Chemotherapy Drugs",
    paste0("K", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Temporary DME Codes",
    paste0("L", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Orthotic Procedures and Devices",
    paste0("M", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Medical Services/Quality Measures",
    # paste0("M", stringr::str_pad(75:301, width = 4, pad = "0")) ~ "Medical Services",
    # paste0("M", stringr::str_pad(1003:1149, width = 4, pad = "0")) ~ "Quality Measures",
    paste0("P", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Pathology and Laboratory",
    paste0("Q", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Miscellaneous Services (Temporary)",
    paste0("R", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Diagnostic Radiology Services",
    paste0("S", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Commercial Payers (Temporary)",
    paste0("T", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "State Medicaid Agency Codes",
    paste0("U", stringr::str_pad(1:9999, width = 4, pad = "0")) ~ "Coronavirus Lab Tests",
    paste0("V", stringr::str_pad(2020:2799, width = 4, pad = "0")) ~ "Vision Services",
    paste0("V", stringr::str_pad(5008:5361, width = 4, pad = "0")) ~ "Hearing Services",
    paste0("V", stringr::str_pad(5362:5364, width = 4, pad = "0")) ~ "Speech-Language Pathology Services",

    .default = NA_character_
    ) |> forcats::as_factor(),
    .after = hcpcs_category
  )

pin_update(
  hcpcs_desc,
  name = "hcpcs_descriptions",
  title = "HCPCS Descriptions Master File",
  description = "HCPCS Descriptions Master File"
)

# A tibble: 5 × 2
# end_letter     n
# 1 A            142 -> 150
# 2 F            565 -> 2233
# 3 M             65 -> 52
# 4 T           2224 -> 2230
# 5 U           1348 -> 1666

hcpcs_desc |>
  hacksaw::filter_split(
    stringr::str_ends(hcpcs_code, stringr::regex("[A]")),
    stringr::str_ends(hcpcs_code, stringr::regex("[F]")),
    stringr::str_ends(hcpcs_code, stringr::regex("[M]")),
    stringr::str_ends(hcpcs_code, stringr::regex("[T]")),
    stringr::str_ends(hcpcs_code, stringr::regex("[U]"))
  )
