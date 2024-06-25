root <- c("C:/Users/Andrew/Desktop/payer_guidelines/data/")

# https://www.cms.gov/medicare/regulations-guidance/physician-self-referral

# Physician Fee Schedule 2024
rvu_xl   <- glue("{root}RVU24A-010323/PPRRVU24_JAN.xlsx")
pay_xl   <- glue("{root}PFREV24A_0/PFALL24.csv")
opps_xl  <- glue("{root}RVU24A-010323/OPPSCAP_JAN.xlsx")
gpci_xl  <- glue("{root}RVU24A-010323/GPCI2024.xlsx")
locco_xl <- glue("{root}RVU24A-010323/24LOCCO.xlsx")
anes_xl  <- glue("{root}RVU24A-010323/ANES2024.xlsx")

# CPT 2023
clinician <- glue("{root}cpt2023/ClinicianDescriptor.xlsx")
consumer  <- glue("{root}cpt2023/ConsumerDescriptor.xlsx")
modifer   <- glue("{root}cpt2023/MODUL.txt")
longuf    <- glue("{root}cpt2023/LONGUF_edit.txt")
longulf   <- glue("{root}cpt2023/LONGULF_edit.txt")
shortuf   <- glue("{root}cpt2023/SHORTUF_edit.txt")
medu      <- glue("{root}cpt2023/MEDU_edit.txt")
level2    <- glue("{root}HCPC2024_APR_ANWEB_v5/HCPC2024_APR_ANWEB_v5.xlsx") # v5

# NCCI Add-on Code Edits
# aoc_paths <- fs::dir_ls(glue::glue("{root}NCCI/"), regexp = "*MCR.xlsx$")
# aoc_names <- aoc_paths |> basename() |> str_remove_all(pattern = fixed(".xlsx"))
# names(aoc_paths) <- aoc_names

# NCCI Medically Unlikely Edits
# mue_paths <- fs::dir_ls("C:/Users/Andrew/Desktop/payer_guidelines/data/NCCI/", regexp = "*2024.xlsx$")
# mue_names <- mue_paths |> basename() |> str_remove_all(pattern = fixed(".xlsx"))
# names(mue_paths) <- mue_names

# NCCI Procedure to Procedure (PTP) Edits
# ptp_paths <- fs::dir_ls("C:/Users/Andrew/Desktop/payer_guidelines/data/NCCI/", regexp = "*[f][0-9].xlsx$")
# ptp_names <- ptp_paths |> basename() |> str_remove_all(pattern = fixed(".xlsx"))
# names(ptp_paths) <- ptp_names

# HCPCS 2024
# procnotes   <- glue::glue("{root}HCPC2024_APR_ANWEB_v5/proc_notes_APR2024.txt")
# level2      <- "C:/Users/Andrew/Desktop/payer_guidelines/data/HCPC2024_JAN_ANWEB_v4/HCPC2024_JAN_ANWEB_v4.xlsx" # v4
# noc_codes   <- glue::glue("{hcpcs}NOC codes_APR 2024.xlsx")
# transreport <- glue::glue("{hcpcs}HCPC2024_APR_Transreport_ANWEB_v5.xlsx")
# corrections <- glue::glue("{hcpcs}HCPC2024_APR_Corrections_to_v5.xlsx")

# LCDs
# lcd_paths <- fs::dir_ls("C:/Users/Andrew/Desktop/all_data/all_lcd/all_lcd_csv/", regexp = "*.csv$")
# lcd_names <- lcd_paths |> basename() |> str_remove_all(pattern = fixed(".csv"))
# names(lcd_paths) <- lcd_names

# NCDs
# ncd_paths <- fs::dir_ls("C:/Users/Andrew/Desktop/all_data/ncd/ncd_csv/", regexp = "*.csv$")
# ncd_names <- ncd_paths |> basename() |> str_remove_all(pattern = fixed(".csv"))
# names(ncd_paths) <- ncd_names
