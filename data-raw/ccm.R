library(googlesheets4)
library(tidyverse)
library(janitor)
library(northstar)

id <- "11iFT9rPKqzu6LgO0sjBtr0K-u5TkrljksoYsTf-hytA"
coding <- read_sheet(id, sheet = "Coding/Reimbursement") |> clean_names()
# resource <- read_sheet(id, sheet = "Resources") |> clean_names()

# gs4_get(gs_id)
# googlesheets4::gs4_browse(gs_id)

coding <- coding |>
  mutate(cpt_hcpcs_code                                  = map(cpt_hcpcs_code, as.character),
         medicare_reimbursement_national                 = map(medicare_reimbursement_national, as.character),
         medicare_patient_responsibility_national        = map(medicare_patient_responsibility_national, as.character),
         non_medicare_participants_charge_limit_self_pay = map(non_medicare_participants_charge_limit_self_pay, as.character),
         mo_health_net                                   = map(mo_health_net, as.character)) |>
  unnest(cols = c(cpt_hcpcs_code,
                  medicare_reimbursement_national,
                  medicare_patient_responsibility_national,
                  non_medicare_participants_charge_limit_self_pay,
                  mo_health_net),
         keep_empty = TRUE) |>
  janitor::remove_empty()

coding <- coding |>
  mutate(medicare_reimbursement_national = readr::parse_number(medicare_reimbursement_national),
         medicare_patient_responsibility_national = readr::parse_number(medicare_patient_responsibility_national),
         non_medicare_participants_charge_limit_self_pay = readr::parse_number(non_medicare_participants_charge_limit_self_pay),
         mo_health_net = dplyr::na_if(mo_health_net, "NA"),
         mo_health_net_prior_auth = dplyr::na_if(mo_health_net_prior_auth, "N/A")) |>
  janitor::remove_empty()

ccm_hcpcs <- coding |>
  filter(!is.na(cpt_hcpcs_code)) |>
  distinct(cpt_hcpcs_code) |>
  pull(cpt_hcpcs_code)

`%chin%` <- data.table::`%chin%`

ccm_cpt <- descriptors() |>
  filter(cpt %chin% ccm_hcpcs) |>
  select(cpt, clinician_descriptor)

cd_99453_98975 <- c("99453: Initial setup and patient education on use of device for remote monitoring of blood pressure")
cd_99454       <- c("99454: Initial supply of device for remote monitoring of blood pressure, with daily recording transmission, per 30 days")
cd_99091       <- c("99091: Collection and interpretation of digital blood glucose monitoring data by other qualified health care professional requiring a minimum of 30 minutes of time, per 30 days")
cd_99457_99458 <- c("99457: Remote physiologic monitoring treatment management services, clinical staff/physician/other qualified health care professional time in a calendar month requiring at least 20 minutes of time, patient initiated, with the following required elements: review of physiologic data, interaction with patient via phone, video, or other digital communication, and generation of a report")
cd_99437_99439_99487_99489_99490_99491 <- c("99490: Chronic care management of multiple chronic conditions with significant risk of death, acute exacerbation/decompensation, or functional decline, comprehensive care plan established, implemented, revised, or monitored; first 20 minutes of clinical staff time directed by qualified health care professional, per calendar month")
cd_99492_99494 <- c("99492: Initial psychiatric collaborative care management, first 70 minutes in first calendar month of behavioral health care manager activities")
cd_98976_98977_98978_98980_98981 <- c("Device supply with daily recording and programmed alert transmission for remote therapeutic monitoring of respiratory system, each 30 days")

ccm_cpt |>
  unite("code", cpt:clinician_descriptor, sep = ": ") |>
  pull(code)

ccm_cpt

ccm_npfs <- hcpcs_search(hcpcs = ccm_hcpcs, state = c("MO", "KS"))

ccm_npfs |>
  remove_empty() |>
  glimpse()

L2 <- hcpcs_lv2() |>
  filter(hcpcs %in% ccm_hcpcs) |>
  remove_empty() |>
  mutate(cov       = reference("hcpcs")$cov[cov],
         tos       = reference("hcpcs")$tos[tos],
         action_cd = reference("hcpcs")$act[action_cd],
         betos     = reference("hcpcs")$betos[betos],
         mult_pi   = reference("hcpcs")$mult[mult_pi],
         price     = reference("hcpcs")$price[price]) |>
  select(hcpcs,
         long_description,
         price,
         mult_pi,
         cov,
         betos,
         tos,
         date_added = add_dt,
         date_effective = act_eff_dt)

ccm_sheet <- ccm_npfs |>
  mutate(
    status     = reference("pfs")$stat[status],
    mult_proc  = reference("pfs")$mult[as.character(mult_proc)],
    global     = reference("pfs")$glob[global],
    pctc       = reference("pfs")$pctc[pctc],
    surg_bilat = reference("pfs")$bil[as.character(surg_bilat)],
    surg_asst  = reference("pfs")$asst[as.character(surg_asst)],
    surg_co    = reference("pfs")$co[as.character(surg_co)],
    surg_team  = reference("pfs")$team[as.character(surg_team)],
    supvis     = reference("pfs")$phys[supvis],
    rare       = reference("pfs")$rare[rare]) |>
  unnest_wider(c(status, pctc), names_sep = "_") |>
  select(-c(flat_vis,
            ntherapy,
            ftherapy,
            op_ind:op_post,
            unused,
            endo,
            dximg,
            opps,
            nprvu_opps,
            mrvu_opps,
            fprvu_opps,
            mult_surg,
            pctc_label)) |>
  left_join(ccm_desc, by = "hcpcs", relationship = "many-to-many") |>
  left_join(L2, by = "hcpcs") |>
  mutate(type = map_chr(hcpcs, label_hcpcs), .after = hcpcs) |>
  select(hcpcs, type, description, mac:tos) |>
  print(n = Inf)

googlesheets4::gs4_create("ccm_sheet_KS", sheets = ccm_sheet)
