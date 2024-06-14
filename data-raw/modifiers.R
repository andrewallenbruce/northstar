source(here::here("data-raw", "source_setup", "setup.R"))

modifiers_cpt <- dplyr::tribble(

  ~modifier, ~modifier_type,   ~modifier_description,
  "22",      "CPT",            "Increased Procedural Services",
  "23",      "CPT",            "Unusual Anesthesia",
  "24",      "CPT",            "Unrelated Evaluation and Management Service by the Same Physician or Other Qualified Health Care Professional During a Postoperative Period",
  "25",      "CPT",            "Significant, Separately Identifiable Evaluation and Management Service by the Same Physician or Other Qualified Health Care Professional on the Same Day of the Procedure or Other Service",
  "26",      "CPT",            "Professional Component",
  "27",      "CPT",            "Multiple Outpatient Hospital E/M Encounters on the Same Date",
  "32",      "CPT",            "Mandated Services",
  "33",      "CPT",            "Preventive Services",
  "47",      "CPT",            "Anesthesia by Surgeon",
  "50",      "CPT",            "Bilateral Procedure",
  "51",      "CPT",            "Multiple Procedures",
  "52",      "CPT",            "Reduced Services",
  "53",      "CPT",            "Discontinued Procedure",
  "54",      "CPT",            "Surgical Care Only",
  "55",      "CPT",            "Postoperative Management Only",
  "56",      "CPT",            "Preoperative Management Only",
  "57",      "CPT",            "Decision for Surgery",
  "58",      "CPT",            "Staged or Related Procedure or Service by the Same Physician or Other Qualified Health Care Professional During the Postoperative Period",
  "59",      "CPT",            "Distinct Procedural Service",
  "62",      "CPT",            "Two Surgeons",
  "63",      "CPT",            "Procedure Performed on Infants Less Than 4 kg",
  "66",      "CPT",            "Surgical Team",
  "73",      "CPT",            "Discontinued Outpatient Hospital/Ambulatory Surgery Center (ASC) Procedure Prior to the Administration of Anesthesia",
  "74",      "CPT",            "Discontinued Outpatient Hospital/Ambulatory Surgery Center (ASC) Procedure After Administration of Anesthesia",
  "76",      "CPT",            "Repeat Procedure by Same Physician or Other Qualified Health Care Professional",
  "77",      "CPT",            "Repeat Procedure by Another Physician or Other Qualified Health Care Professional",
  "78",      "CPT",            "Unplanned Return to the Operating/Procedure Room by the Same Physician or Other Qualified Health Care Professional Following Initial Procedure for a Related Procedure During the Postoperative Period",
  "79",      "CPT",            "Unrelated Procedure or Service by the Same Physician or Other Qualified Health Care Professional During the Postoperative Period",
  "80",      "CPT",            "Assistant Surgeon",
  "81",      "CPT",            "Minimum Assistant Surgeon",
  "82",      "CPT",            "Assistant Surgeon (when qualified resident surgeon not available)",
  "90",      "CPT",            "Reference (Outside) Laboratory",
  "91",      "CPT",            "Repeat Clinical Diagnostic Laboratory Test",
  "92",      "CPT",            "Alternative Laboratory Platform Testing",
  "93",      "CPT",            "Synchronous Telemedicine Service Rendered Via Telephone or Other Real-Time Interactive Audio-Only Telecommunications System",
  "95",      "CPT",            "Synchronous Telemedicine Service Rendered Via a Real-Time Interactive Audio and Video Telecommunications System",
  "96",      "CPT",            "Habilitative Services",
  "97",      "CPT",            "Rehabilitative Services",
  "99",      "CPT",            "Multiple Modifiers"
)

modifiers_cpt_information <- dplyr::tribble(

  ~modifier,  ~modifier_information,
  "22",       "When the work required to provide a service is substantially greater than typically required, it may be identified by adding modifier 22 to the usual procedure code. Documentation must support the substantial additional work and the reason for the additional work (i.e., increased intensity, time, technical difficulty of procedure, severity of patient's condition, physical and mental effort required). Note: This modifier should not be appended to an E/M service.",
  "23",       "Occasionally, a procedure, which usually requires either no anesthesia or local anesthesia, because of unusual circumstances must be done under general anesthesia. This circumstance may be reported by adding modifier 23 to the procedure code of the basic service.",
  "24",       "The physician or other qualified health care professional may need to indicate that an evaluation and management service was performed during a postoperative period for a reason(s) unrelated to the original procedure. This circumstance may be reported by adding modifier 24 to the appropriate level of E/M service.",
  "25",       "It may be necessary to indicate that on the day a procedure or service identified by a CPT code was performed, the patient's condition required a significant, separately identifiable E/M service above and beyond the other service provided or beyond the usual preoperative and postoperative care associated with the procedure that was performed. A significant, separately identifiable E/M service is defined or substantiated by documentation that satisfies the relevant criteria for the respective E/M service to be reported (see Evaluation and Management Services Guidelines for instructions on determining level of E/M service). The E/M service may be prompted by the symptom or condition for which the procedure and/or service was provided. As such, different diagnoses are not required for reporting of the E/M services on the same date. This circumstance may be reported by adding modifier 25 to the appropriate level of E/M service. Note: This modifier is not used to report an E/M service that resulted in a decision to perform surgery (see modifier 57). For significant, separately identifiable non-E/M services, see modifier 59.",
  "26",       "Certain procedures are a combination of a physician or other qualified health care professional component and a technical component. When the physician or other qualified health care professional component is reported separately, the service may be identified by adding modifier 26 to the usual procedure number.",
  "27",       "For hospital outpatient reporting purposes, utilization of hospital resources related to separate and distinct E/M encounters performed in multiple outpatient hospital settings on the same date may be reported by adding modifier 27 to each appropriate level outpatient and/or emergency department E/M code(s). This modifier provides a means of reporting circumstances involving evaluation and management services provided by physician(s) in more than one (multiple) outpatient hospital setting(s) (e.g., hospital emergency department, clinic). Note: This modifier is not to be used for physician reporting of multiple E/M services performed by the same physician on the same date. For physician reporting of all outpatient evaluation and management services provided by the same physician on the same date and performed in multiple outpatient setting(s) (e.g., hospital emergency department, clinic), see Evaluation and Management, Emergency Department, or Preventive Medicine Services codes.",
  "32",       "Services related to mandated consultation and/or related services (eg, third party payer, governmental, legislative or regulatory requirement) may be identified by adding modifier 32 to the basic procedure.",
  "33",       "When the primary purpose of the service is the delivery of an evidence based service in accordance with a US Preventive Services Task Force A or B rating in effect and other preventive services identified in preventive services mandates (legislative or regulatory), the service may be identified by adding 33 to the procedure. For separately reported services specifically identified as preventive, the modifier should not be used.",
  "47",       "Regional or general anesthesia provided by the surgeon may be reported by adding modifier 47 to the basic service (This does not include local anesthesia.) Note: Modifier 47 would not be used as a modifier for the anesthesia procedures.",
  "50",       "Unless otherwise identified in the listings, bilateral procedures that are performed at the same session, should be identified by adding modifier 50 to the appropriate 5 digit code. Note: This modifier should not be appended to designated add-on codes.",
  "51",       "When multiple procedures, other than E/M services, Physical Medicine and Rehabilitation services or provision of supplies (e.g., vaccines), are performed at the same session by the same individual, the primary procedure or service may be reported as listed. The additional procedure(s) or service(s) may be identified by appending modifier 51 to the additional procedure or service code(s). Note: This modifier should not be appended to designated add-on codes.",
  "52",       "Under certain circumstances a service or procedure is partially reduced or eliminated at the discretion of the physician or other qualified health care professional. Under these circumstances the service provided can be identified by its usual procedure number and the addition of modifier 52, signifying that the service is reduced. This provides a means of reporting reduced services without disturbing the identification of the basic service. Note: For hospital outpatient reporting of a previously scheduled procedure/service that is partially reduced or cancelled as a result of extenuating circumstances or those that threaten the well-being of the patient prior to or after administration of anesthesia, see modifiers 73 and 74 (see modifiers approved for ASC hospital outpatient use).",
  "53",       "Under certain circumstances, the physician or other qualified health care professional may elect to terminate a surgical or diagnostic procedure. Due to extenuating circumstances or those that threaten the well being of the patient, it may be necessary to indicate that a surgical or diagnostic procedure was started but discontinued. This circumstance may be reported by adding modifier 53 to the code reported by the individual for the discontinued procedure. Note: This modifier is not used to report the elective cancellation of a procedure prior to the patient's anesthesia induction and/or surgical preparation in the operating suite. For outpatient hospital/ambulatory surgery center (ASC) reporting of a previously scheduled procedure/service that is partially reduced or cancelled as a result of extenuating circumstances or those that threaten the well being of the patient prior to or after administration of anesthesia, see modifiers 73 and 74 (see modifiers approved for ASC hospital outpatient use).",
  "54",       "When 1 physician or other qualified health care professional performs a surgical procedure and another provides preoperative and/or postoperative management, surgical services may be identified by adding modifier 54 to the usual procedure number.",
  "55",       "When 1 physician or other qualified health care professional performed the postoperative management and another performed the surgical procedure, the postoperative component may be identified by adding modifier 55 to the usual procedure number.",
  "56",       "When 1 physician or other qualified health care professional performed the preoperative care and evaluation and another performed the surgical procedure, the preoperative component may be identified by adding modifier 56 to the usual procedure number.",
  "57",       "An evaluation and management service that resulted in the initial decision to perform the surgery may be identified by adding modifier 57 to the appropriate level of E/M service.",
  "58",       "It may be necessary to indicate that the performance of a procedure or service during the postoperative period was: (a) planned or anticipated (staged); (b) more extensive than the original procedure; or (c) for therapy following a surgical procedure. This circumstance may be reported by adding modifier 58 to the staged or related procedure. Note: For treatment of a problem that requires a return to the operating/procedure room (e.g., unanticipated clinical condition), see modifier 78.",
  "59",       "Under certain circumstances, it may be necessary to indicate that a procedure or service was distinct or independent from other non-E/M services performed on the same day. Modifier 59 is used to identify procedures/services, other than E/M services, that are not normally reported together, but are appropriate under the circumstances. Documentation must support a different session, different procedure or surgery, different site or organ system, separate incision/excision, separate lesion, or separate injury (or area of injury in extensive injuries) not ordinarily encountered or performed on the same day by the same individual. However, when another already established modifier is appropriate it should be used rather than modifier 59. Only if no more descriptive modifier is available, and the use of modifier 59 best explains the circumstances, should modifier 59 be used. Note: Modifier 59 should not be appended to an E/M service. To report a separate and distinct E/M service with a non-E/M service performed on the same date, see modifier 25.",
  "62",       "When 2 surgeons work together as primary surgeons performing distinct part(s) of a procedure, each surgeon should report his/her distinct operative work by adding modifier 62 to the procedure code and any associated add-on code(s) for that procedure as long as both surgeons continue to work together as primary surgeons. Each surgeon should report the co-surgery once using the same procedure code. If additional procedure(s) (including add-on procedure(s) are performed during the same surgical session, separate code(s) may also be reported with modifier 62 added. Note: If a co-surgeon acts as an assistant in the performance of additional procedure(s), other than those reported with the modifier 62, during the same surgical session, those services may be reported using separate procedure code(s) with modifier 80 or modifier 82 added, as appropriate.",
  "63",       "Procedures performed on neonates and infants up to a present body weight of 4 kg may involve significantly increased complexity and physician or other qualified health care professional work commonly associated with these patients. This circumstance may be reported by adding modifier 63 to the procedure number. Note: Unless otherwise designated, this modifier may only be appended to procedures/services listed in the 20100-69990 code series and 92920, 92928, 92953, 92960, 92986, 92987, 92990, 92997, 92998, 93312, 93313, 93314, 93315, 93316, 93317, 93318, 93452, 93505, 93563, 93564, 93568, 93569, 93573, 93574, 93575, 93580, 93581, 93582, 93590, 93591, 93592, 93593, 93594, 93595, 93596, 93597, 93598, 93615, 93616 from the Medicine/Cardiovascular section. Modifier 63 should not be appended to any CPT codes listed in the Evaluation and Management Services, Anesthesia, Radiology, Pathology and Laboratory, or Medicine sections (other than those identified above from the Medicine/Cardiovascular section).",
  "66",       "Under some circumstances, highly complex procedures (requiring the concomitant services of several physicians or other qualified health care professionals, often of different specialties, plus other highly skilled, specially trained personnel, various types of complex equipment) are carried out under the surgical team concept. Such circumstances may be identified by each participating individual with the addition of modifier 66 to the basic procedure number used for reporting services.",
  "73",       "Due to extenuating circumstances or those that threaten the well being of the patient, the physician may cancel a surgical or diagnostic procedure subsequent to the patient's surgical preparation (including sedation when provided, and being taken to the room where the procedure is to be performed), but prior to the administration of anesthesia (local, regional block(s) or general). Under these circumstances, the intended service that is prepared for but cancelled can be reported by its usual procedure number and the addition of modifier 73. Note: The elective cancellation of a service prior to the administration of anesthesia and/or surgical preparation of the patient should not be reported. For physician reporting of a discontinued procedure, see modifier 53.",
  "74",       "Due to extenuating circumstances or those that threaten the well being of the patient, the physician may terminate a surgical or diagnostic procedure after the administration of anesthesia (local, regional block(s), general) or after the procedure was started (incision made, intubation started, scope inserted, etc). Under these circumstances, the procedure started but terminated can be reported by its usual procedure number and the addition of modifier 74. Note: The elective cancellation of a service prior to the administration of anesthesia and/or surgical preparation of the patient should not be reported. For physician reporting of a discontinued procedure, see modifier 53.",
  "76",       "It may be necessary to indicate that a procedure or service was repeated by the same physician or other qualified health care professional subsequent to the original procedure or service. This circumstance may be reported by adding modifier 76 to the repeated procedure or service. Note: This modifier should not be appended to an E/M service.",
  "77",       "It may be necessary to indicate that a basic procedure or service was repeated by another physician or other qualified health care professional subsequent to the original procedure or service. This circumstance may be reported by adding modifier 77 to the repeated procedure or service. Note: This modifier should not be appended to an E/M service.",
  "78",       "It may be necessary to indicate that another procedure was performed during the postoperative period of the initial procedure (unplanned procedure following initial procedure). When this procedure is related to the first, and requires the use of an operating/procedure room, it may be reported by adding modifier 78 to the related procedure. For repeat procedures, see modifier 76.",
  "79",       "The individual may need to indicate that the performance of a procedure or service during the postoperative period was unrelated to the original procedure. This circumstance may be reported by using modifier 79. For repeat procedures on the same day, see modifier 76.",
  "80",       "Surgical assistant services may be identified by adding modifier 80 to the usual procedure number(s).",
  "81",       "Minimum surgical assistant services are identified by adding modifier 81 to the usual procedure number.",
  "82",       "The unavailability of a qualified resident surgeon is a prerequisite for use of modifier 82 appended to the usual procedure code number(s).",
  "90",       "When laboratory procedures are performed by a party other than the treating or reporting physician or other qualified health care professional, the procedure may be identified by adding modifier 90 to the usual procedure number.",
  "91",       "In the course of treatment of the patient, it may be necessary to repeat the same laboratory test on the same day to obtain subsequent (multiple) test results. Under these circumstances, the laboratory test performed can be identified by its usual procedure number and the addition of modifier 91. Note: This modifier may not be used when tests are rerun to confirm initial results; due to testing problems with specimens or equipment; or for any other reason when a normal, one-time, reportable result is all that is required. This modifier may not be used when other code(s) describe a series of test results (e.g., glucose tolerance tests, evocative/suppression testing). This modifier may only be used for laboratory test(s) performed more than once on the same day on the same patient.",
  "92",       "When laboratory testing is being performed using a kit or transportable instrument that wholly or in part consists of a single use, disposable analytical chamber, the service may be identified by adding modifier 92 to the usual laboratory procedure code (HIV testing 86701-86703, and 87389). The test does not require permanent dedicated space, hence by its design may be hand carried or transported to the vicinity of the patient for immediate testing at that site, although location of the testing is not in itself determinative of the use of this modifier.",
  "93",       "Synchronous telemedicine service is defined as a real-time interaction between a physician or other qualified health care professional and a patient who is located away at a distant site from the physician or other qualified health care professional. The totality of the communication of information exchanged between the physician or other qualified health care professional and the patient during the course of the synchronous telemedicine service must be of an amount and nature that is sufficient to meet the key components and/or requirements of the same service when rendered via a face-to-face interaction.",
  "95",       "Synchronous telemedicine service is defined as a real-time interaction between a physician or other qualified health care professional and a patient who is located at a distant site from the physician or other qualified health care professional. The totality of the communication of information exchanged between the physician or other qualified health care professional and the patient during the course of the synchronous telemedicine service must be of an amount and nature that would be sufficient to meet the key components and/or requirements of the same service when rendered via a face-to-face interaction. Modifier 95 may only be appended to the services listed in Appendix P. Appendix P is the list of CPT codes for services that are typically performed face-to-face, but may be rendered via a real-time (synchronous) interactive audio and video telecommunications system.",
  "96",       "When a service or procedure that may be either habilitative or rehabilitative in nature is provided for habilitative purposes, the physician or other qualified health care professional may add modifier 96 to the service or procedure code to indicate that the service or procedure provided was a habilitative service. Habilitative services help an individual learn skills and functioning for daily living that the individual has not yet developed, and then keep and/or improve those learned skills. Habilitative services also help an individual keep, learn, or improve skills and functioning for daily living.",
  "97",       "When a service or procedure that may be either habilitative or rehabilitative in nature is provided for rehabilitative purposes, the physician or other qualified health care professional may add modifier 97 to the service or procedure code to indicate that the service or procedure provided was a rehabilitative service. Rehabilitative services help an individual keep, get back, or improve skills and functioning for daily living that have been lost or impaired because the individual was sick, hurt, or disabled.",
  "99",       "Under certain circumstances 2 or more modifiers may be necessary to completely delineate a service. In such situations modifier 99 should be added to the basic procedure, and other applicable modifiers may be listed as part of the description of the service."
)

# CPT Modifier Join
modifiers_cpt <- dplyr::left_join(
  modifiers_cpt,
  modifiers_cpt_information,
  by = dplyr::join_by(modifier))


# HCPCS Level II Modifiers
modifiers_hcpcs <- northstar:::get_pin("two_mods") |>
  dplyr::mutate(
    two_desc_long = stringr::str_remove_all(two_desc_long, '"')
  ) |>
  dplyr::reframe(
    modifier = hcpcs,
    modifier_type = "HCPCS",
    modifier_description = two_desc_long)

modifiers_hcpcs |>
  print(n = 400)

# Anesthesia Modifiers
modifiers_anesthesia <- dplyr::tribble(
  ~modifier,  ~modifier_type,  ~modifier_description,                                                              ~modifier_information,
  "P1",       "Anesthesia",   "A normal healthy patient",                                                         "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier.",
  "P2",       "Anesthesia",   "A patient with mild systemic disease",                                             "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier.",
  "P3",       "Anesthesia",   "A patient with severe systemic disease",                                           "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier.",
  "P4",       "Anesthesia",   "A patient with severe systemic disease that is a constant threat to life",         "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier.",
  "P5",       "Anesthesia",   "A moribund patient who is not expected to survive without the operation",          "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier.",
  "P6",       "Anesthesia",   "A declared brain-dead patient whose organs are being removed for donor purposes",  "Physical Status modifiers are consistent with American Society of Anesthesiologists ranking of patient physical status, and distinguishing various levels of complexity of the anesthesia service provided. All anesthesia services are reported by use of the anesthesia five-digit procedure codes (00100-01999) with the appropriate physical status modifier appended. Under certain circumstances, when another established modifier(s) is appropriate, it should be used in addition to the physical status modifier."
)


# Performance Measure Modifiers
modifiers_performance <- dplyr::tribble(
  ~modifier, ~modifier_type,         ~modifier_description,                                                                               ~modifier_information,
  "1P",      "Performance Measure",  "Performance Measure Exclusion Modifier due to Medical Reasons",                                     "Reasons include: Not indicated (absence of organ/limb, already received/ performed, other); Contraindicated (patient allergic history, potential adverse drug interaction, other); Other medical reasons",
  "2P",      "Performance Measure",  "Performance Measure Exclusion Modifier due to Patient Reasons",                                     "Reasons include: Patient declined; Economic, social, or religious reasons; Other patient reasons",
  "3P",      "Performance Measure",  "Performance Measure Exclusion Modifier due to System Reasons",                                      "Reasons include: Resources to perform the services not available; Insurance coverage/payor-related limitations; Other reasons attributable to health care delivery system",
  "8P",      "Performance Measure",  "Performance Measure Reporting Modifier, action not performed, reason not otherwise specified",      "Modifier 8P is intended to be used as a reporting modifier to allow the reporting of circumstances when an action described in a measure's numerator is not performed and the reason is not otherwise specified."
)

modifiers <- vctrs::vec_rbind(
  modifiers_cpt,
  modifiers_hcpcs,
  modifiers_anesthesia,
  modifiers_performance
) |>
  dplyr::mutate(
    modifier_type = forcats::as_factor(modifier_type))

modifiers |>
  filter(modifier %in% c("73", "74"))

# Like CPT modifiers, you want to list the
# HCPCS modifier that directly affects
# reimbursement (the “functional modifier”)
# before the one that merely provides
# information about the procedure (the “informational modifier”)


# Modifiers can be two digit numbers,
# two character modifiers, or alpha-numeric
# indicators. Modifiers provide additional
# information to payers to make sure your
# provider gets paid correctly for services
# rendered. If appropriate, more than one
# modifier may be used with a single procedure
# code; however, are not applicable for every
# category of the CPT codes. Some modifiers
# can only be used with a particular category
# and some are not compatible with others.
#
# https://med.noridianmedicare.com/web/jeb/topics/modifiers

mod_names <- c(
  "General",
  "Advance_Beneficiary_Notice_of_Noncoverage_ABN",

  "Ambulance_Origin_Destination",
  # Auto Denied Modifiers - DD, DE, DP, DR, DS, ED, EE, EP, ER, ES, GD, GG, GI,
  # GJ, GP, GS, GX, HD, HG, HP, HS, HX, ID, IE, IJ, IN, IP, IR, IS, IX, JD, JG,
  # JI, JJ, JP, JS, JX, NI, NN, NP, NS, PD, PE, PG, PJ, PN, PP, PR, PS, PX, RD,
  # RE, RP, RR, RS, SD, SE, SG, SJ, SN, SP, SR, SS, XD, XE, XG, XJ, XN, XP, XR,
  # XS, XX. Trips with one of these origin/destination modifiers are not covered
  # and should not be submitted to Medicare. A provider may bill the patient
  # directly for these services. If a provider must bill Medicare for a denial,
  # append modifier GY.

  "Anatomic__Side_of_Body",
  "Anatomic__Eyelid",
  "Anatomic__Hand",
  "Anatomic__Feet",
  "Anatomic__Coronary_Artery",

  # Anesthesia modifiers are used to receive the correct payment of anesthesia
  # services. Pricing modifiers must be placed in the first modifier field to
  # ensure proper payment (AA, AD, QK, QX, QY, and QZ). Informational modifiers
  # are used in conjunction with pricing modifiers and must be placed in the
  # second modifier position (QS, G8, G9, and 23).
  "Anesthesia",

  "Assist_at_Surgery",
  "Chiropractic",

  # If a drug meets the definition of "usually self-administered," Noridian will
  # determine that the drug does not meet a Medicare benefit category. The use
  # of the JA and JB modifiers is required for drugs which have one HCPCS Level
  # II (J or Q) code but multiple routes of administration. Drugs that fall
  # under this category must be billed with the JA modifier for the intravenous
  # infusion of the drug or billed with the JB modifier for the subcutaneous
  # injection form of administration. Noridian presumes that drugs delivered
  # intravenously are not usually self-administered by the patient.
  # -----
  # CMS requires providers with claims for drugs or biologicals from single use
  # vials or single use packages appropriately discarded to submit claims with
  # unused portions or indicate there was zero amount unused. The units billed
  # must correspond with the smallest dose (vial) available for purchase from
  # the manufacturer(s) providing the appropriate patient dose, while minimizing
  # any wastage.
  "Drugs_and_Biologicals__Administration",
  "Drugs_and_Biologicals__Disposal",

  "Physician_Quality_Reporting_System_PQRS",
  "Telehealth",
  "Therapy"
)

# names <- rvest::read_html(
#   "https://med.noridianmedicare.com/web/jeb/topics/modifiers"
#   ) |>
#   rvest::html_elements("a") |>
#   rvest::html_attr("href") |>
#   stringr::str_subset("#") |>
#   stringr::str_subset("main", negate = TRUE) |>
#   stringr::str_remove_all("#") |>
#   purrr::discard(\(x) x == "")

tbls <- rvest::read_html(
  "https://med.noridianmedicare.com/web/jeb/topics/modifiers") |>
  rvest::html_elements(".table") |>
  rvest::html_table() |>
  purrr::set_names(mod_names)

tbls[4:8] <- purrr::map(tbls[4:8], ~ {
  .x |>
  tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::pivot_wider(names_from = name, values_from = value, values_fn = list) |>
    tidyr::unnest(c(Modifier, `Modifier Description`))
})

tbls <- purrr:::map(tbls, ~ {
  .x |>
    janitor::clean_names() |>
    dplyr::mutate(
      modifier = stringr::str_remove_all(modifier, "[Mm]od "),
      modifier = dplyr::na_if(modifier, ""),
      modifier_description = stringr::str_remove_all(modifier_description, '"')
      ) |>
    dplyr::filter(
      !is.na(modifier),
      modifier != "P1 – P6 P1 P2 P3 P4 P5 P6"
      )
}) |>
  purrr::list_rbind(names_to = "modifier_category") |>
  dplyr::select(
    modifier,
    modifier_category,
    modifier_description
    )

partial_urls <- rvest::read_html(
  "https://med.noridianmedicare.com/web/jeb/topics/modifiers") |>
  rvest::html_elements(".table") |>
  html_elements("a") |>
  html_attr("href") |>
  unique()

vec_urls <- glue::glue("https://med.noridianmedicare.com{partial_urls}")

url <- vec_urls[1]

scrape_urls <- function(url) {

  pg <- rvest::read_html(url)

  x <- list(
    name = pg |>
      rvest::html_elements(".title") |>
      rvest::html_text2(preserve_nbsp = TRUE) |>
      stringr::str_subset("Browse by Topic", negate = TRUE),
    info = pg |>
      rvest::html_elements(".portlet-journal-content .journal-content-article") |>
      rvest::html_elements("h3, ul") |>
      rvest::html_text2(preserve_nbsp = FALSE) |>
      tibble::enframe(
        name = "row",
        value = "text"
      ) |>
      dplyr::mutate(row = NULL) |>
      tidyr::separate_longer_delim(
        text,
        delim = "\n"
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        heading = stringr::str_extract(
          text,
          "Instructions|Introduction|Definition|Correct Use|Appropriate Usage|Incorrect Use|Inappropriate Usage|Special Appeals Process|Resource|Additional Information"
        ), .before = text
      ) |>
      tidyr::fill(heading) |>
      dplyr::filter(
        stringr::str_detect(
          text,
          "Instructions|Introduction|Definition|Correct Use|Appropriate Usage|Incorrect Use|Inappropriate Usage|Special Appeals Process|Resource|Additional Information",
          negate = TRUE
        )
      )
  )

  dplyr::tibble(
    modifier = stringr::str_remove_all(x$name, "Modifier "),
    instructions = list(x$info)
  )
}

tictoc::tic()
modifier_pages <- purrr::map(
  vec_urls,
  scrape_urls
)
tictoc::toc()

modifier_pages <- modifier_pages |>
  purrr::list_rbind() |>
  tidyr::unnest(instructions) |>
  dplyr::mutate(heading = dplyr::case_match(
    heading,
    "Appropriate Usage" ~ "Correct Use",
    "Inappropriate Usage" ~ "Incorrect Use",
    c("Resource", "Special Appeals Process") ~ "Additional Information",
    .default = heading
  )) |>
  tidyr::pivot_wider(
    names_from = heading,
    values_from = text,
    values_fn = list
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    instructions = purrr::map_chr(
    instructions, ~ paste(.x, collapse = "; ")),
    instructions = dplyr::na_if(instructions, ""),

    additional_information = purrr::map_chr(
    additional_information, ~ paste(.x, collapse = ". ")),
    additional_information = dplyr::na_if(additional_information, ""),

    correct_use = purrr::map_chr(
    correct_use, ~ paste(.x, collapse = ". ")),
    correct_use = dplyr::na_if(correct_use, ""),
    correct_use = stringr::str_remove_all(correct_use, '"'),

    incorrect_use = purrr::map_chr(
    incorrect_use, ~ paste(.x, collapse = ". ")),
    incorrect_use = dplyr::na_if(incorrect_use, ""),
    incorrect_use = stringr::str_remove_all(incorrect_use, '"')
  ) |>
  dplyr::select(
    modifier,
    correct_use,
    incorrect_use,
    instructions,
    additional_information
  )

mod_info <- tbls |>
  dplyr::full_join(
    modifier_pages,
    by = dplyr::join_by(modifier)
  ) |>
  dplyr::rename(
    modifier_correct_use = correct_use,
    modifier_incorrect_use = incorrect_use,
    modifier_instructions = instructions,
    modifier_additional_information = additional_information
  )

modifiers <- modifiers |>
  dplyr::rename(
    modifier_detailed_information = modifier_information
    ) |>
  dplyr::left_join(
    mod_info,
    by = dplyr::join_by(modifier)
  ) |>
  dplyr::mutate(
    modifier_description = dplyr::if_else(
      is.na(modifier_description.y),
      modifier_description.x,
      modifier_description.y),
    modifier_description.x = NULL,
    modifier_description.y = NULL,
    .after = modifier_type
  ) |>
  dplyr::select(
    modifier,
    modifier_type,
    modifier_category,
    modifier_description,
    modifier_detailed_information,
    modifier_correct_use,
    modifier_incorrect_use,
    modifier_instructions,
    modifier_additional_information
  )

# Update Pin
pin_update(
  modifiers,
  name = "modifiers",
  title = "HCPCS Modifiers",
  description = "Level I and II HCPCS Modifiers"
)
