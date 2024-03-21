mods <- dplyr::tribble(
  ~mod, ~description,
  "25", "Significant, Separately Identifiable Evaluation and Management Service by the Same Physician or Other Qualified Health Care Professional on the Same Day of the Procedure or Other Service",
  "59", "Distinct Procedural Service",
  "95", "Synchronous Telemedicine Service Rendered Via a Real-Time Interactive Audio and Video Telecommunications System",
  "AH", "Clinical Psychologist",
  "AJ", "Clinical Social Worker",
  "G0", "Telehealth Services for Diagnosis, Evaluation, or Treatment of Symptoms of an Acute Stroke",
  "GQ", "Telehealth Service Rendered Via Asynchronous Telecommunications System",
  "GT", "Telehealth Service Rendered Via Interactive Audio and Video Telecommunications System",
  "HE", "Mental Health Program",
  "HF", "Substance Abuse Program",
  "HG", "Opioid Addiction Treatment Program",
  "HH", "Integrated Mental Health/Substance Abuse Program",
  "HI", "Integrated Mental Health and Intellectual Disability/Developmental Disabilities Program",
  "HJ", "Employee Assistance Program",
  "HK", "Specialized Mental Health Programs for High-Risk Populations",
  "HQ", "Group Setting",
  "HR", "Family/Couple with Client Present",
  "HS", "Family/Couple without Client Present",
  "U8", "Medicaid Level of Care 8, as Defined by Each State",
  "UD", "Medicaid Level of Care 13, as Defined by Each State",
)

# Update Pin
board <- pins::board_folder(here::here("pins"))

board |>
  pins::pin_write(mods,
                  name = "modifiers",
                  title = "HCPCS Modifiers",
                  description = "Level I and II HCPCS Modifiers",
                  type = "qs")

board |> pins::write_board_manifest()
