# https://www.cms.gov/medicare/coding-billing/place-of-service-codes/code-sets
#
# Database (updated September 2023) Listed below are place of service codes and
# descriptions. These codes should be used on professional claims to specify the
# entity where service(s) were rendered. Check with individual payers (e.g.,
# Medicare, Medicaid, other private insurance) for reimbursement policies
# regarding these codes. NOTE:  Please direct questions related to billing place
# of service codes to your Medicare Administrative Contractor (MAC) for
# assistance.

library(tidyverse)
library(pdftools)

pos_pdf <- "C:/Users/Andrew/Desktop/payer_guidelines/data/Website_POS_database__9_21_23.pdf"

pos_1 <- stringr::str_split(pdf_text(pos_pdf)[1], "\n")

pos_1[[1]][17:37]

cat(pdf_text(pos_pdf))

pdf_data(pos_pdf)[[1]] |>
  print(n = 50)


place_of_service_codes <- tribble(
  ~pos_code, ~pos_description,
  "01", "Pharmacy",
  "02", "Telehealth",
  "03", "School",
  "04", "Homeless Shelter",
  "05", "Indian Health Service Free-standing Facility",
  "06", "Indian Health Service Provider-based Facility",
  "07", "Tribal 638 Free-standing Facility",
  "08", "Tribal 638 Provider-based Facility",
  "09", "Prison/Correctional Facility",
  "11", "Office",
  "12", "Home",
  "13", "Assisted Living Facility",
  "14", "Group Home",
  "15", "Mobile Unit",
  "16", "Temporary Lodging",
  "17", "Walk-in Retail Health Clinic",
  "18", "Place of Employment/Worksite",
  "19", "Off Campus-Outpatient Hospital",
  "20", "Urgent Care Facility",
  "21", "Inpatient Hospital",
  "22", "Outpatient Hospital",
  "23", "Emergency Room-Hospital",
  "24", "Ambulatory Surgical Center",
  "25", "Federal Qualified Health Center",
  "26", "Rural Health Clinic",
  "31", "Skilled Nursing Facility",
  "32", "Nursing Facility",
  "33", "Custodial Care Facility",
  "34", "Hospice",
  "41", "Ambulance-Land",
  "42", "Ambulance-Air or Water",
  "49", "Independent Clinic",
  "50", "Federally Qualified Health Center Look-Alike",
  "51", "Inpatient Psychiatric Facility",
  "52", "Psychiatric Facility-Partial Hospitalization",
  "53", "Community Mental Health Center",
  "54", "Intermediate Care Facility/Mentally Retarded",
  "55", "Residential Substance Abuse Treatment Facility",
  "56", "Psychiatric Residential Treatment Center",
  "57", "Non-residential Substance Abuse Treatment Facility",
  "60", "Mass Immunization Center",
  "61", "Comprehensive Inpatient Rehabilitation Facility",
  "62", "Comprehensive Outpatient Rehabilitation Facility",
  "65", "End-Stage Renal Disease Treatment Facility",
  "71", "Public Health Clinic",
  "72", "Rural Health"
  )

# https://pdftables.com/pdf-to-excel-api
pdftable_api <- "ylfao7io33l7"
