source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))
source(here::here("data-raw", "case_labeling_functions.R"))

# NATIONAL PHYSICIAN FEE SCHEDULE RELATIVE VALUE FILE CALENDAR YEAR 2024
rvu <- read_excel(
  rvu_xl,
  col_types = "text") |>
  row_to_names(row_number = 9) |>
  clean_names() |>
  mutate(
    across(
      c(
      work_rvu,
      non_fac_pe_rvu,
      facility_pe_rvu,
      non_facility_total,
      facility_total,
      mp_rvu,
      conv_factor,
      pre_op,
      intra_op,
      post_op,
      non_facility_pe_used_for_opps_payment_amount,
      facility_pe_used_for_opps_payment_amount,
      mp_used_for_opps_payment_amount
    ),
    readr::parse_number
  )) |>
  select(
    hcpcs,
    rel_desc          = description,
    rel_status        = status_code,
    rel_wrvu          = work_rvu,
    rel_mrvu          = mp_rvu,
    rel_prvu_non      = non_fac_pe_rvu,
    rel_prvu_fac      = facility_pe_rvu,
    rel_conv          = conv_factor,
    rel_non_tot       = non_facility_total,
    rel_fac_tot       = facility_total,
    rel_opps_prvu_non = non_facility_pe_used_for_opps_payment_amount,
    rel_opps_prvu_fac = facility_pe_used_for_opps_payment_amount,
    rel_opps_mrvu     = mp_used_for_opps_payment_amount,
    rel_global        = glob_days,
    rel_preop         = pre_op,
    rel_intraop       = intra_op,
    rel_postop        = post_op,
    rel_surg_bilat    = bilat_surg,
    rel_surg_asst     = asst_surg,
    rel_surg_co       = co_surg,
    rel_surg_team     = team_surg,
    rel_mult_proc     = mult_proc,
    rel_mod           = mod,
    rel_pctc          = pctc_ind,
    rel_endo          = endo_base,
    rel_dr_viz        = physician_supervision_of_diagnostic_procedures,
    rel_img_fam       = diagnostic_imaging_family_indicator,
    rel_non_na        = non_fac_na_indicator,
    rel_fac_na        = facility_na_indicator,
    rel_not_used      = not_used_for_medicare_payment,
    rel_calc_flag     = calculation_flag) |>
  mutate(rel_op_ind   = rel_preop + rel_intraop + rel_postop, .before = rel_preop)

rvu$rel_conv <- as.double(rvu$rel_conv)

# HCPCS and RVU short descriptions
# A tibble: 18,500 × 2
rvu_descriptions <- rvu |> dplyr::select(hcpcs, rel_desc)

pin_update(
  rvu_descriptions,
  name = "hcpcs_rvu_descriptions",
  title = "HCPCS Descriptions from RVU 2024 File",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with RVUs
# A tibble: 9,220 × 8
hcpcs_with_rvus <- rvu |>
  dplyr::select(
    hcpcs,
    mod = rel_mod,
    # pctc = rel_pctc,
    rvu_work = rel_wrvu,
    rvu_mp = rel_mrvu,
    rvu_pe_non = rel_prvu_non,
    rvu_pe_fac = rel_prvu_fac,
    # conv_fct = rel_conv, # 32.7442
    rvu_tot_non = rel_non_tot,
    rvu_tot_fac = rel_fac_tot
  )

hcpcs_with_rvus |>
  dplyr::filter(
    rvu_tot_non > 0 | rvu_tot_fac > 0
    )

hcpcs_gt_1 <- hcpcs_with_rvus |>
  count(hcpcs, sort = TRUE) |>
  filter(n > 1) |>
  pull(hcpcs)

hcpcs_with_rvus |>
  dplyr::filter(hcpcs %in% hcpcs_gt_1) |>
  dplyr::filter(
    tot_non > 0 | tot_fac > 0
  )

pin_update(
  hcpcs_with_rvus,
  name = "hcpcs_with_rvus",
  title = "HCPCS with RVUs",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with valid PCTC Indicator
# A tibble: 3,667 × 4
hcpcs_with_pctc <- rvu |>
  dplyr::select(
    hcpcs,
    rel_pctc) |>
  dplyr::filter(!is.na(rel_pctc)) |>
  dplyr::filter(rel_pctc != "9") |>
  # dplyr::filter(rel_pctc != "9", rel_pctc != "0") |>
  case_pctc(rel_pctc) |>
  dplyr::select(
    hcpcs,
    pctc_ind = rel_pctc,
    pctc_group = pctc_label,
    pctc_description)

pin_update(
  hcpcs_with_pctc,
  name = "hcpcs_with_pctc",
  title = "HCPCS with valid PCTC Indicator",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with Mod indicator
# A tibble: 2,176 × 4
hcpcs_with_mod <- rvu |>
  dplyr::select(
    hcpcs,
    rel_mod) |>
  dplyr::filter(!is.na(rel_mod)) |>
  case_modifier(rel_mod) |>
  dplyr::select(
    hcpcs,
    mod_ind = rel_mod,
    mod_group = mod_label,
    mod_description)

pin_update(
  hcpcs_with_mod,
  name = "hcpcs_with_mod",
  title = "HCPCS with Mod indicator",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with Surgical Indicators
# A tibble: 10,629 × 9
hcpcs_surg_ind <- rvu |>
  dplyr::select(
    hcpcs,
    rel_surg_bilat,
    rel_surg_asst,
    rel_surg_co,
    rel_surg_team) |>
  dplyr::filter(rel_surg_bilat != "9" | rel_surg_asst != "9" | rel_surg_co != "9" | rel_surg_team != "9") |>
  dplyr::select(
    hcpcs,
    surg_bilat_ind = rel_surg_bilat,
    surg_asst_ind = rel_surg_asst,
    surg_co_ind = rel_surg_co,
    surg_team_ind = rel_surg_team) |>
  case_assistant(surg_asst_ind) |>
  case_cosurg(surg_co_ind) |>
  case_team(surg_team_ind) |>
  case_bilateral(surg_bilat_ind) |>
  dplyr::rename(
    surg_bilat_desc = bilat_description,
    surg_asst_desc = asst_description,
    surg_co_desc = cosurg_description,
    surg_team_desc = team_description)

pin_update(
  hcpcs_surg_ind,
  name = "hcpcs_surg_ind",
  title = "HCPCS with Surgical Indicators",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS Main Indicators: Status Code, Global Days, Op Ind, Multiple Procedure
# A tibble: 18,500 × 5
hcpcs_main_ind <- rvu |>
  dplyr::select(
    hcpcs,
    status_code = rel_status,
    global_days = rel_global,
    op_ind = rel_op_ind,
    mult_proc_ind = rel_mult_proc
  )

pin_update(
  hcpcs_main_ind,
  name = "hcpcs_main_ind",
  title = "HCPCS Main Indicators: Status Code, Global Days, Op Ind, Multiple Procedure",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with OPPS
# A tibble: 1,036 × 4
hcpcs_with_opps_rvus <- rvu |>
  dplyr::select(
    hcpcs,
    opps_non_prvu = rel_opps_prvu_non,
    opps_fac_prvu = rel_opps_prvu_fac,
    opps_mrvu = rel_opps_mrvu
  ) |>
  dplyr::filter(
    opps_non_prvu > 0 | opps_fac_prvu > 0 | opps_mrvu > 0
    )

pin_update(
  hcpcs_with_opps_rvus,
  name = "hcpcs_with_opps_rvus",
  title = "HCPCS with OPPS RVUs",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with Operative Percentages
# A tibble: 4,230 × 4
hcpcs_with_op_pct <- rvu |>
  dplyr::select(
    hcpcs,
    rel_op_ind,
    rel_preop,
    rel_intraop,
    rel_postop
  ) |>
  dplyr::filter(rel_op_ind == 1) |>
  dplyr::select(
    hcpcs,
    op_pre = rel_preop,
    op_intra = rel_intraop,
    op_post = rel_postop
    )

pin_update(
  hcpcs_with_op_pct,
  name = "hcpcs_with_op_pct",
  title = "HCPCS with Operative Percentages",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS rarely or never performed in a non-facility setting
rare_non <- rvu |>
  dplyr::select(
    hcpcs,
    rel_non_na
  ) |>
  dplyr::filter(!is.na(rel_non_na)) |>
  dplyr::mutate(rare_never = "Non-Facility",
                rel_non_na = NULL)

# HCPCS rarely or never performed in a facility setting
rare_fac <- rvu |>
  dplyr::select(
    hcpcs,
    rel_fac_na
  ) |>
  dplyr::filter(!is.na(rel_fac_na)) |>
  dplyr::mutate(rare_never = "Facility",
                rel_fac_na = NULL)

# A tibble: 6,534 × 2
hcpcs_rare <- vctrs::vec_rbind(
  rare_non,
  rare_fac
)

pin_update(
  hcpcs_rare,
  name = "hcpcs_rare",
  title = "HCPCS rarely or never performed in a facility or nonfacility setting",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# Vector of HCPCS not used for Medicare payment
# length(hcpcs_not_used) = 133
hcpcs_not_used <- rvu |>
  dplyr::select(
    hcpcs,
    rel_not_used) |>
  dplyr::filter(!is.na(rel_not_used)) |>
  dplyr::pull(hcpcs)

pin_update(
  hcpcs_not_used,
  name = "hcpcs_not_used",
  title = "Vector of HCPCS not used for Medicare payment",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)


# Endoscopic Base Code for HCPCS with Multiple Surgery indicator 3
# A tibble: 361 × 2
hcpcs_endo <- rvu |>
  dplyr::select(
    hcpcs,
    rel_endo) |>
  dplyr::filter(!is.na(rel_endo)) |>
  dplyr::rename(
    endoscopic_base = rel_endo
  )

pin_update(
  hcpcs_endo,
  name = "hcpcs_endo",
  title = "Endoscopic Base Code for HCPCS with Multiple Surgery indicator 3",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# Diagnostic Imaging Family Indicator
# Diagnostic Service Family for HCPCS with Multiple Procedure indicator 4
# A tibble: 444 × 3
hcpcs_dximg <- rvu |>
  dplyr::select(
    hcpcs,
    rel_img_fam) |>
  dplyr::filter(!is.na(rel_img_fam)) |>
  dplyr::filter(rel_img_fam != "99") |>
  # case_dximg(rel_img_fam) |>
  dplyr::rename(dx_img_fam_ind = rel_img_fam) |>
  dplyr::mutate(
    dx_img_fam_desc = dplyr::case_match(
      dx_img_fam_ind,
      "01" ~ "Ultrasound: Chest/Abdomen/Pelvis (Non-Obstetrical)",
      "02" ~ "CT/CTA: Chest/Thorax/Abdomen/Pelvis",
      "03" ~ "CT/CTA: Head/Brain/Orbit/Maxillofacial/Neck",
      "04" ~ "MRI/MRA: Chest/Abdomen/Pelvis",
      "05" ~ "MRI/MRA: Head/Brain/Neck",
      "06" ~ "MRI/MRA: Spine",
      "07" ~ "CT: Spine",
      "08" ~ "MRI/MRA: Lower Extremities",
      "09" ~ "CT/CTA: Lower Extremities",
      "10" ~ "MRI/MRA: Upper Extremities and Joints",
      "11" ~ "CT/CTA: Upper Extremities",
      "88" ~ "Subject to TC/PC Reduction",
      "99" ~ NA_character_
    ))

pin_update(
  hcpcs_dximg,
  name = "hcpcs_dximg",
  title = "Diagnostic Service Family for HCPCS with Multiple Procedure indicator 4",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)

# HCPCS with Physician Supervision of Diagnostic Procedures Indicators
# A tibble: 1,034 × 3
hcpcs_with_phys_supvision <- rvu |>
  dplyr::select(
    hcpcs,
    rel_dr_viz) |>
  dplyr::filter(!is.na(rel_dr_viz)) |>
  dplyr::filter(rel_dr_viz != "09") |>
  case_supervision(rel_dr_viz) |>
  dplyr::select(
    hcpcs,
    phys_vis_ind = rel_dr_viz,
    phys_vis_description = supvis_description)

pin_update(
  hcpcs_with_phys_supvision,
  name = "hcpcs_with_phys_vis",
  title = "HCPCS with Physician Supervision of Diagnostic Procedures Indicators",
  description = "National Physician Fee Schedule Relative Value File January 2024 Release"
)


# Update Pin
# pin_update(
#   rvu,
#   name = "rvu",
#   title = "PFS RVU 2024",
#   description = "National Physician Fee Schedule Relative Value File January 2024 Release"
# )
#
delete_pins("hcpcs_with_phys_supvision")
list_pins()
