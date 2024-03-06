library(googlesheets4)
library(tidyverse)
library(janitor)
library(northstar)

id <- "11iFT9rPKqzu6LgO0sjBtr0K-u5TkrljksoYsTf-hytA"
coding <- read_sheet(id, sheet = "Coding/Reimbursement") |> clean_names()
resource <- read_sheet(id, sheet = "Resources") |> clean_names()

# gs4_get(gs_id)
# googlesheets4::gs4_browse(gs_id)

ccm <- coding |>
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
  rename(hcpcs           = cpt_hcpcs_code,
         reimbursement   = medicare_reimbursement_national,
         pat_resp        = medicare_patient_responsibility_national,
         sp_limit        = non_medicare_participants_charge_limit_self_pay,
         mohn            = mo_health_net,
         mohn_auth       = mo_health_net_prior_auth,
         short           = short_description,
         long            = long_description,
         criteria        = criteria_for_selecting_code,
         mue_value      = medicare_mue_values,
         mue_adjud       = medicare_mue_adjudication_indicator,
         mue_reason      = medicare_mue_rationale,
         )

ccm_hcpcs <- ccm |>
  filter(!is.na(hcpcs)) |>
  distinct(hcpcs) |>
  pull(hcpcs)


ccm_npfs <- calc_amounts_df(hcpcs = ccm_hcpcs, state = "MO")


ccm_desc <- cpt_descriptors() |>
  filter(cpt %in% c(ccm_hcpcs)) |>
  # select(-clinician_descriptor) |>
  distinct() |>
  rename(hcpcs = cpt,
         consumer_desc = consumer_descriptor,
         clinician_desc = clinician_descriptor)

L2 <- hcpcs() |>
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

ccm_npfs |>
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
