source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))

# https://www.cms.gov/medicare/coding-billing/place-of-service-codes/code-sets
#
# Database (updated September 2023) Listed below are place of service codes and
# descriptions. These codes should be used on professional claims to specify the
# entity where service(s) were rendered. Check with individual payers (e.g.,
# Medicare, Medicaid, other private insurance) for reimbursement policies
# regarding these codes. NOTE:  Please direct questions related to billing place
# of service codes to your Medicare Administrative Contractor (MAC) for
# assistance.
#
# https://www.cms.gov/regulations-and-guidance/guidance/manuals/downloads/clm104c26pdf.pdf
# https://www.hhs.gov/guidance/sites/default/files/hhs-guidance-documents/mm7631.pdf

place_of_service_codes <- dplyr::tribble(
  ~pos_type,       ~pos_code, ~pos_name,                                                                 ~pos_description,
  "Non-Facility",  "01",      "Pharmacy",                                                               "A facility or location where drugs and other medically related items and services are sold, dispensed, or otherwise provided directly to patients.",
  "Facility",      "02",      "Telehealth Provided Other than in Patient’s Home",                       "The location where health services and health related services are provided or received, through telecommunication technology. Patient is not located in their home when receiving health services or health related services through telecommunication technology.",
  "Non-Facility",  "03",      "School",                                                                 "A facility whose primary purpose is education.",
  "Non-Facility",  "04",      "Homeless Shelter",                                                       "A facility or location whose primary purpose is to provide temporary housing to homeless individuals (e.g., emergency shelters, individual or family shelters).",
  NA,              "05",      "Indian Health Service Free-standing Facility",                           "A facility or location, owned and operated by the Indian Health Service, which provides diagnostic, therapeutic (surgical and non-surgical), and rehabilitation services to American Indians and Alaska Natives who do not require hospitalization.",
  NA,              "06",      "Indian Health Service Provider-based Facility",                          "A facility or location, owned and operated by the Indian Health Service, which provides diagnostic, therapeutic (surgical and non-surgical), and rehabilitation services rendered by, or under the supervision of, physicians to American Indians and Alaska Natives admitted as inpatients or outpatients.",
  NA,              "07",      "Tribal 638 Free-standing Facility",                                      "A facility or location owned and operated by a federally recognized American Indian or Alaska Native tribe or tribal organization under a 638 agreement, which provides diagnostic, therapeutic (surgical and non-surgical), and rehabilitation services to tribal members who do not require hospitalization.",
  NA,              "08",      "Tribal 638 Provider-based Facility",                                     "A facility or location owned and operated by a federally recognized American Indian or Alaska Native tribe or tribal organization under a 638 agreement, which provides diagnostic, therapeutic (surgical and non-surgical), and rehabilitation services to tribal members admitted as inpatients or outpatients.",
  "Non-Facility",  "09",      "Prison/Correctional Facility",                                           "A prison, jail, reformatory, work farm, detention center, or any other similar facility maintained by either Federal, State or local authorities for the purpose of confinement or rehabilitation of adult or juvenile criminal offenders.",
  NA,              "10",      "Telehealth Provided in Patient’s Home",                                  "The location where health services and health related services are provided or received, through telecommunication technology. Patient is located in their home (which is a location other than a hospital or other facility where the patient receives care in a private residence) when receiving health services or health related services through telecommunication technology.",
  "Non-Facility",  "11",      "Office",                                                                 "Location, other than a hospital, skilled nursing facility (SNF), military treatment facility, community health center, State or local public health clinic, or intermediate care facility (ICF), where the health professional routinely provides health examinations, diagnosis, and treatment of illness or injury on an ambulatory basis.",
  "Non-Facility",  "12",      "Home",                                                                   "Location, other than a hospital or other facility, where the patient receives care in a private residence.",
  "Non-Facility",  "13",      "Assisted Living Facility",                                               "Congregate residential facility with self-contained living units providing assessment of each resident's needs and on-site support 24 hours a day, 7 days a week, with the capacity to deliver or arrange for services including some health care and other services.",
  "Non-Facility",  "14",      "Group Home",                                                             "A residence, with shared living areas, where clients receive supervision and other services such as social and/or behavioral services, custodial service, and minimal services (e.g., medication administration).",
  "Non-Facility",  "15",      "Mobile Unit",                                                            "A facility/unit that moves from place-to-place equipped to provide preventive, screening, diagnostic, and/or treatment services.",
  "Non-Facility",  "16",      "Temporary Lodging",                                                      "A short term accommodation such as a hotel, camp ground, hostel, cruise ship or resort where the patient receives care, and which is not identified by any other POS code.",
  "Non-Facility",  "17",      "Walk-in Retail Health Clinic",                                           "A walk-in health clinic, other than an office, urgent care facility, pharmacy or independent clinic and not described by any other Place of Service code, that is located within a retail operation and provides, on an ambulatory basis, preventive and primary care services.",
  NA,              "18",      "Place of Employment/Worksite",                                           "A location, not described by any other POS code, owned or operated by a public or private entity where the patient is employed, and where a health professional provides on-going or episodic occupational medical, therapeutic or rehabilitative services to the individual.",
  "Facility",      "19",      "Off Campus-Outpatient Hospital",                                         "A portion of an off-campus hospital provider based department which provides diagnostic, therapeutic (both surgical and nonsurgical), and rehabilitation services to sick or injured persons who do not require hospitalization or institutionalization.",
  "Non-Facility",  "20",      "Urgent Care Facility",                                                   "Location, distinct from a hospital emergency room, an office, or a clinic, whose purpose is to diagnose and treat illness or injury for unscheduled, ambulatory patients seeking immediate medical attention.",
  "Facility",      "21",      "Inpatient Hospital",                                                     "A facility, other than psychiatric, which primarily provides diagnostic, therapeutic (both surgical and nonsurgical), and rehabilitation services by, or under, the supervision of physicians to patients admitted for a variety of medical conditions.",
  "Facility",      "22",      "On Campus-Outpatient Hospital",                                          "A portion of a hospital’s main campus which provides diagnostic, therapeutic (both surgical and nonsurgical), and rehabilitation services to sick or injured persons who do not require hospitalization or institutionalization.",
  "Facility",      "23",      "Emergency Room-Hospital",                                                "A portion of a hospital where emergency diagnosis and treatment of illness or injury is provided.",
  "Facility",      "24",      "Ambulatory Surgical Center",                                             "A freestanding facility, other than a physician's office, where surgical and diagnostic services are provided on an ambulatory basis.",
  "Non-Facility",  "25",      "Birthing Center",                                                        "A facility, other than a hospital's maternity facilities or a physician's office, which provides a setting for labor, delivery, and immediate post-partum care as well as immediate care of new born infants.",
  "Facility",      "26",      "Military Treatment Facility",                                            "A medical facility operated by one or more of the Uniformed Services. Military Treatment Facility (MTF) also refers to certain former U.S. Public Health Service (USPHS) facilities now designated as Uniformed Service Treatment Facilities (USTF).",
  "Non-Facility",  "27",      "Outreach Site/Street",                                                   "A non-permanent location on the street or found environment, not described by any other POS code, where health professionals provide preventive, screening, diagnostic, and/or treatment services to unsheltered homeless individuals.",
  NA,              "28",      "Unassigned",                                                             NA,
  NA,              "29",      "Unassigned",                                                             NA,
  NA,              "30",      "Unassigned",                                                             NA,
  "Facility",      "31",      "Skilled Nursing Facility",                                               "A facility which primarily provides inpatient skilled nursing care and related services to patients who require medical, nursing, or rehabilitative services but does not provide the level of care or treatment available in a hospital.",
  "Non-Facility",  "32",      "Nursing Facility",                                                       "A facility which primarily provides to residents skilled nursing care and related services for the rehabilitation of injured, disabled, or sick persons, or, on a regular basis, health-related care services above the level of custodial care to other than individuals with intellectual disabilities.",
  "Non-Facility",  "33",      "Custodial Care Facility",                                                "A facility which provides room, board and other personal assistance services, generally on a long-term basis, and which does not include a medical component.",
  "Facility",      "34",      "Hospice",                                                                "A facility, other than a patient's home, in which palliative and supportive care for terminally ill patients and their families are provided.",
  NA,              "35",      "Unassigned",                                                             NA,
  NA,              "36",      "Unassigned",                                                             NA,
  NA,              "37",      "Unassigned",                                                             NA,
  NA,              "38",      "Unassigned",                                                             NA,
  NA,              "39",      "Unassigned",                                                             NA,
  NA,              "40",      "Unassigned",                                                             NA,
  "Facility",      "41",      "Ambulance-Land",                                                         "A land vehicle specifically designed, equipped and staffed for lifesaving and transporting the sick or injured.",
  "Facility",      "42",      "Ambulance-Air or Water",                                                 "An air or water vehicle specifically designed, equipped and staffed for lifesaving and transporting the sick or injured.",
  NA,              "43",      "Unassigned",                                                             NA,
  NA,              "44",      "Unassigned",                                                             NA,
  NA,              "45",      "Unassigned",                                                             NA,
  NA,              "46",      "Unassigned",                                                             NA,
  NA,              "47",      "Unassigned",                                                             NA,
  NA,              "48",      "Unassigned",                                                             NA,
  "Non-Facility",  "49",      "Independent Clinic",                                                     "A location, not part of a hospital and not described by any other Place of Service code, that is organized and operated to provide preventive, diagnostic, therapeutic, rehabilitative, or palliative services to outpatients only.",
  "Non-Facility",  "50",      "Federally Qualified Health Center",                                      "A facility located in a medically underserved area that provides Medicare beneficiaries preventive primary medical care under the general direction of a physician.",
  "Facility",      "51",      "Inpatient Psychiatric Facility",                                         "A facility that provides inpatient psychiatric services for the diagnosis and treatment of mental illness on a 24-hour basis, by or under the supervision of a physician.",
  "Facility",      "52",      "Psychiatric Facility-Partial Hospitalization",                           "A facility for the diagnosis and treatment of mental illness that provides a planned therapeutic program for patients who do not require full time hospitalization, but who need broader programs than are possible from outpatient visits to a hospital-based or hospital-affiliated facility.",
  "Facility",      "53",      "Community Mental Health Center",                                         "A facility that provides the following services: outpatient services, including specialized outpatient services for children, the elderly, individuals who are chronically ill, and residents of the CMHC's mental health services area who have been discharged from inpatient treatment at a mental health facility; 24 hour a day emergency care services; day treatment, other partial hospitalization services, or psychosocial rehabilitation services; screening for patients being considered for admission to State mental health facilities to determine the appropriateness of such admission; and consultation and education services.",
  "Non-Facility",  "54",      "Intermediate Care Facility/Individuals with Intellectual Disabilities",  "A facility which primarily provides health-related care and services above the level of custodial care to individuals but does not provide the level of care or treatment available in a hospital or SNF.",
  "Non-Facility",  "55",      "Residential Substance Abuse Treatment Facility",                         "A facility which provides treatment for substance (alcohol and drug) abuse to live-in residents who do not require acute medical care. Services include individual and group therapy and counseling, family counseling, laboratory tests, drugs and supplies, psychological testing, and room and board.",
  "Facility",      "56",      "Psychiatric Residential Treatment Center",                               "A facility or distinct part of a facility for psychiatric care which provides a total 24-hour therapeutically planned and professionally staffed group living and learning environment.",
  "Non-Facility",  "57",      "Non-residential Substance Abuse Treatment Facility",                     "A location which provides treatment for substance (alcohol and drug) abuse on an ambulatory basis.  Services include individual and group therapy and counseling, family counseling, laboratory tests, drugs and supplies, and psychological testing.",
  "Non-Facility",  "58",      "Non-residential Opioid Treatment Facility",                              "A location that provides treatment for opioid use disorder on an ambulatory basis. Services include methadone and other forms of Medication Assisted Treatment (MAT)",
  NA,              "59",      "Unassigned",                                                             NA,
  "Non-Facility",  "60",      "Mass Immunization Center",                                               "A location where providers administer pneumococcal pneumonia and influenza virus vaccinations and submit these services as electronic media claims, paper claims, or using the roster billing method. This generally takes place in a mass immunization setting, such as, a public health center, pharmacy, or mall but may include a physician office setting.",
  "Facility",      "61",      "Comprehensive Inpatient Rehabilitation Facility",                        "A facility that provides comprehensive rehabilitation services under the supervision of a physician to inpatients with physical disabilities. Services include physical therapy, occupational therapy, speech pathology, social or psychological services, and orthotics and prosthetics services.",
  "Non-Facility",  "62",      "Comprehensive Outpatient Rehabilitation Facility",                       "A facility that provides comprehensive rehabilitation services under the supervision of a physician to outpatients with physical disabilities. Services include physical therapy, occupational therapy, and speech pathology services.",
  NA,              "63",      "Unassigned",                                                             NA,
  NA,              "64",      "Unassigned",                                                             NA,
  "Non-Facility",  "65",      "End-Stage Renal Disease Treatment Facility",                             "A facility other than a hospital, which provides dialysis treatment, maintenance, and/or training to patients or caregivers on an ambulatory or home-care basis.",
  "Non-Facility",  "66",      "Programs of All-Inclusive Care for the Elderly (PACE) Center",           "A facility or location providing comprehensive medical and social services as part of the Programs of All-Inclusive Care for the Elderly (PACE). This includes, but is not limited to, primary care; social work services; restorative therapies, including physical and occupational therapy; personal care and supportive services; nutritional counseling; recreational therapy; and meals when the individual is enrolled in PACE.",
  NA,              "67",      "Unassigned",                                                             NA,
  NA,              "68",      "Unassigned",                                                             NA,
  NA,              "69",      "Unassigned",                                                             NA,
  NA,              "70",      "Unassigned",                                                             NA,
  "Non-Facility",  "71",      "State or Local Public Health Clinic",                                    "A facility maintained by either State or local health departments that provides ambulatory primary medical care under the general direction of a physician.",
  "Non-Facility",  "72",      "Rural Health",                                                           "A certified facility which is located in a rural medically underserved area that provides ambulatory primary medical care under the general direction of a physician.",
  NA,              "73",      "Unassigned",                                                             NA,
  NA,              "74",      "Unassigned",                                                             NA,
  NA,              "75",      "Unassigned",                                                             NA,
  NA,              "76",      "Unassigned",                                                             NA,
  NA,              "77",      "Unassigned",                                                             NA,
  NA,              "78",      "Unassigned",                                                             NA,
  NA,              "79",      "Unassigned",                                                             NA,
  NA,              "80",      "Unassigned",                                                             NA,
  "Non-Facility",  "81",      "Independent Laboratory",                                                 "A laboratory certified to perform diagnostic and/or clinical tests independent of an institution or a physician's office.",
  NA,              "82",      "Unassigned",                                                             NA,
  NA,              "83",      "Unassigned",                                                             NA,
  NA,              "84",      "Unassigned",                                                             NA,
  NA,              "85",      "Unassigned",                                                             NA,
  NA,              "86",      "Unassigned",                                                             NA,
  NA,              "87",      "Unassigned",                                                             NA,
  NA,              "88",      "Unassigned",                                                             NA,
  NA,              "89",      "Unassigned",                                                             NA,
  NA,              "90",      "Unassigned",                                                             NA,
  NA,              "91",      "Unassigned",                                                             NA,
  NA,              "92",      "Unassigned",                                                             NA,
  NA,              "93",      "Unassigned",                                                             NA,
  NA,              "94",      "Unassigned",                                                             NA,
  NA,              "95",      "Unassigned",                                                             NA,
  NA,              "96",      "Unassigned",                                                             NA,
  NA,              "97",      "Unassigned",                                                             NA,
  NA,              "98",      "Unassigned",                                                             NA,
  "Non-Facility",  "99",      "Other Place of Service",                                                 "Other place of service not identified above."
  ) |>
  dplyr::mutate(pos_type = forcats::as_factor(pos_type)) |>
  dplyr::select(
    pos_code,
    pos_type,
    pos_name,
    pos_description
  )

# Update Pin
pin_update(
  place_of_service_codes,
  name        = "pos_codes",
  title       = "Place of Service Codes",
  description = "Place of Service Codes and Descriptions"
)
