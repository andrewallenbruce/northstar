library(readxl)
library(tidyverse)
library(janitor)

level2 <- "C:/Users/Andrew/Desktop/payer_guidelines/data/HCPC2024_JAN_ANWEB_v4/HCPC2024_JAN_ANWEB_v4.xlsx"

two <- read_excel(level2, col_types = "text") |>
  clean_names() |>
  remove_empty() |>
  rename(hcpcs = hcpc) |>
  mutate(len = str_length(hcpcs), .after = hcpcs) |>
  mutate(type = if_else(len == 2, "mod", "code"), .after = hcpcs) |>
  mutate(len = NULL) |>
  mutate(add_dt = convert_to_date(add_dt),
         act_eff_dt = convert_to_date(act_eff_dt),
         term_dt = convert_to_date(term_dt))

two |>
  count(price1)

board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(two,
                  name = "hcpcs",
                  title = "2024 HCPCS Level II",
                  description = "2024 Healthcare Common Procedure Coding System (HCPCS)",
                  type = "qs")

board |> pins::write_board_manifest()


coverage <- c(
  "C" = "Carrier Judgment",
  "D" = "Special Coverage Instructions Apply",
  "I" = "Not Payable by Medicare",
  "M" = "Non-Covered by Medicare",
  "S" = "Non-Covered by Medicare Statute"
)

# HCPCS Level 1
# Category I: numbers only, except for G0402, G0438, G0439
# Category II: 5 digits, ending in F
# Category III: 5 digits, ending in T
#
# HCPCS Level 2
# Level II codes are composed of a single letter [A-V], followed by 4 digits.
# A = Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental
lk_lvl2 <- c(
  "A" = "Transportation, Medical & Surgical Supplies, Miscellaneous & Experimental",
  "B" = "Enteral and Parenteral Therapy",
  "C" = "Temporary Hospital Outpatient Prospective Payment System",
  "D" = "Dental Procedures",
  "E" = "Durable Medical Equipment",
  "G" = "Temporary Procedures & Professional Services",
  "H" = "Rehabilitative Services",
  "J" = "Drugs Administered Other Than Oral Method, Chemotherapy Drugs",
  "K" = "Temporary Codes for Durable Medical Equipment Regional Carriers",
  "L" = "Orthotic/Prosthetic Procedures",
  "M" = "Medical Services",
  "P" = "Pathology and Laboratory",
  "Q" = "Temporary Codes",
  "R" = "Diagnostic Radiology Services",
  "S" = "Private Payer Codes",
  "T" = "State Medicaid Agency Codes",
  "V" = "Vision/Hearing Services"
)

# BETOS
betos <- c("D1A" = "Medical/surgical supplies",
  "D1B" = "Hospital beds",
  "D1C" = "Oxygen and supplies",
  "D1D" = "Wheelchairs",
  "D1E" = "Other DME",
  "D1F" = "Prosthetic/Orthotic devices",
  "D1G" = "Drugs Administered through DME",
  "I1A" = "Standard imaging - chest",
  "I1B" = "Standard imaging - musculoskeletal",
  "I1C" = "Standard imaging - breast",
  "I1D" = "Standard imaging - contrast gastrointestinal",
  "I1E" = "Standard imaging - nuclear medicine",
  "I1F" = "Standard imaging - other",
  "I2A" = "Advanced imaging - CAT/CT/CTA: brain/head/neck",
  "I2B" = "Advanced imaging - CAT/CT/CTA: other",
  "I2C" = "Advanced imaging - MRI/MRA: brain/head/neck",
  "I2D" = "Advanced imaging - MRI/MRA: other",
  "I3A" = "Echography/ultrasonography - eye",
  "I3B" = "Echography/ultrasonography - abdomen/pelvis",
  "I3C" = "Echography/ultrasonography - heart",
  "I3D" = "Echography/ultrasonography - carotid arteries",
  "I3E" = "Echography/ultrasonography - prostate, transrectal",
  "I3F" = "Echography/ultrasonography - other",
  "I4A" = "Imaging/procedure - heart including cardiac catheterization",
  "I4B" = "Imaging/procedure - other",
  "M1A" = "Office visits - new",
  "M1B" = "Office visits - established",
  "M2A" = "Hospital visit - initial",
  "M2B" = "Hospital visit - subsequent",
  "M2C" = "Hospital visit - critical care",
  "M3 " = "Emergency room visit",
  "M4A" = "Home visit",
  "M4B" = "Nursing home visit",
  "M5A" = "Specialist - pathology",
  "M5B" = "Specialist - psychiatry",
  "M5C" = "Specialist - opthamology",
  "M5D" = "Specialist - other",
  "M6 " = "Consultations",
  "O1A" = "Ambulance",
  "O1B" = "Chiropractic",
  "O1C" = "Enteral and parenteral",
  "O1D" = "Chemotherapy",
  "O1E" = "Other drugs",
  "O1F" = "Hearing and speech services",
  "O1G" = "Immunizations/Vaccinations",
  "01L" = "Lymphedema Compression Treatment Items (eff 1/1/2024)",
  "P0 " = "Anesthesia",
  "P1A" = "Major procedure - breast",
  "P1B" = "Major procedure - colectomy",
  "P1C" = "Major procedure - cholecystectomy",
  "P1D" = "Major procedure - turp",
  "P1E" = "Major procedure - hysterectomy",
  "P1F" = "Major procedure - explor/decompr/excisdisc",
  "P1G" = "Major procedure - other",
  "P2A" = "Major procedure, cardiovascular-CABG",
  "P2B" = "Major procedure, cardiovascular-Aneurysm repair",
  "P2C" = "Major Procedure, cardiovascular-Thromboendarterectomy",
  "P2D" = "Major procedure, cardiovascular-Coronary angioplasty (PTCA)",
  "P2E" = "Major procedure, cardiovascular-Pacemaker insertion",
  "P2F" = "Major procedure, cardiovascular-Other",
  "P3A" = "Major procedure, orthopedic - Hip fracture repair",
  "P3B" = "Major procedure, orthopedic - Hip replacement",
  "P3C" = "Major procedure, orthopedic - Knee replacement",
  "P3D" = "Major procedure, orthopedic - other",
  "P4A" = "Eye procedure - corneal transplant",
  "P4B" = "Eye procedure - cataract removal/lens insertion",
  "P4C" = "Eye procedure - retinal detachment",
  "P4D" = "Eye procedure - treatment of retinal lesions",
  "P4E" = "Eye procedure - other",
  "P5A" = "Ambulatory procedures - skin",
  "P5B" = "Ambulatory procedures - musculoskeletal",
  "P5C" = "Ambulatory procedures - inguinal hernia repair",
  "P5D" = "Ambulatory procedures - lithotripsy",
  "P5E" = "Ambulatory procedures - other",
  "P6A" = "Minor procedures - skin",
  "P6B" = "Minor procedures - musculoskeletal",
  "P6C" = "Minor procedures - other (Medicare fee schedule)",
  "P6D" = "Minor procedures - other (Non-Medicare fee schedule)",
  "P7A" = "Oncology - radiation therapy",
  "P7B" = "Oncology - other",
  "P8A" = "Endoscopy - arthroscopy",
  "P8B" = "Endoscopy - upper gastrointestinal",
  "P8C" = "Endoscopy - sigmoidoscopy",
  "P8D" = "Endoscopy - colonoscopy",
  "P8E" = "Endoscopy - cystoscopy",
  "P8F" = "Endoscopy - bronchoscopy",
  "P8G" = "Endoscopy - laparoscopic cholecystectomy",
  "P8H" = "Endoscopy - laryngoscopy",
  "P8I" = "Endoscopy - other",
  "P9A" = "Dialysis services (Medicare fee schedule)",
  "P9B" = "Dialysis services (Non-Medicare fee schedule)",
  "T1A" = "Lab tests - routine venipuncture (Non-Medicare fee schedule)",
  "T1B" = "Lab tests - automated general profiles",
  "T1C" = "Lab tests - urinalysis",
  "T1D" = "Lab tests - blood counts",
  "T1E" = "Lab tests - glucose",
  "T1F" = "Lab tests - bacterial cultures",
  "T1G" = "Lab tests - other (Medicare fee schedule)",
  "T1H" = "Lab tests - other (Non-Medicare fee schedule)",
  "T2A" = "Other tests - electrocardiograms",
  "T2B" = "Other tests - cardiovascular stress tests",
  "T2C" = "Other tests - EKG monitoring",
  "T2D" = "Other tests - other",
  "Y1"  = "Other - Medicare fee schedule",
  "Y2"  = "Other - Non-Medicare fee schedule",
  "Z1"  = "Local codes",
  "Z2"  = "Undefined codes")
