# Category I Sections
list(
  "Evaluation and Management" = as.character(99202:99499),
  "Anesthesiology"            = c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140),
  "Surgery"                   = as.character(10004:69990),
  "Radiology"                 = as.character(70010:79999),
  "Pathology & Laboratory"    = c(80047:89398, stringr::str_pad(paste0(1:999, "U"), width = 5, pad = "0")),
  "Medicine"                  = as.character(c(90281:99199, 99500:99607)),
  "Immunization"              = paste0(stringr::str_pad(1:999, width = 4, pad = "0"), "A"),
)

list(
  "Evaluation and Management" = "99202 - 99499",
  "Anesthesiology"            = "00100 - 01999, 99100 - 99140",
  "Surgery"                   = "10004 - 69990",
  "Radiology"                 = "70010 - 79999",
  "Pathology & Laboratory"    = "80047 - 89398, 0001U - 0363U",
  "Medicine"                  = "90281 - 99199, 99500 - 99607",
  "Immunization"              = "0001A - 0112A"
)

range(as.character(c(stringr::str_pad(100:1999, width = 5, pad = "0"), 99100:99140)))

# Category I Immunization Codes
# (A codes)
# https://www.ama-assn.org/practice-management/cpt/category-i-immunization-codes

# Category I Proprietary Laboratory Analyses (PLA) Codes
# (U codes)
# https://www.ama-assn.org/practice-management/cpt/cpt-pla-codes

# Category I Molecular Pathology Tier 2 Codes
# https://www.ama-assn.org/practice-management/cpt/molecular-pathology-tier-2-codes

# Category I Administrative MAAA Codes (Multianalyte Assays With Algorithmic Analyses Codes)
# (M codes)
# https://www.ama-assn.org/practice-management/cpt/multianalyte-assays-algorithmic-analyses-codes

# Category II Performance Measurement Codes
# (F codes)
# https://www.ama-assn.org/practice-management/cpt/category-ii-codes
range(paste0(as.character(stringr::str_pad(1:999, width = 4, pad = "0")), "F"))

list(
  "Composite Measures"                             = paste0(stringr::str_pad(1:15, width = 4, pad = "0"), "F"),
  "Patient Management"                             = paste0(stringr::str_pad(500:584, width = 4, pad = "0"), "F"),
  "Patient History"                                = paste0(stringr::str_pad(1000:1505, width = 4, pad = "0"), "F"),
  "Physical Examination"                           = paste0(stringr::str_pad(2000:2060, width = 4, pad = "0"), "F"),
  "Diagnostic/Screening Processes or Results"      = paste0(stringr::str_pad(3006:3776, width = 4, pad = "0"), "F"),
  "Therapeutic, Preventive or Other Interventions" = paste0(stringr::str_pad(4000:4563, width = 4, pad = "0"), "F"),
  "Follow-Up or Other Outcomes"                    = paste0(stringr::str_pad(5005:5250, width = 4, pad = "0"), "F"),
  "Patient Safety"                                 = paste0(stringr::str_pad(6005:6150, width = 4, pad = "0"), "F"),
  "Structural Measures"                            = paste0(stringr::str_pad(7010:7025, width = 4, pad = "0"), "F"),
  "Non-Measure Claims Based Reporting"             = paste0(stringr::str_pad(9001:9007, width = 4, pad = "0"), "F"),
)

# Category III Temporary Codes (T codes)
# https://www.ama-assn.org/practice-management/cpt/category-iii-codes
