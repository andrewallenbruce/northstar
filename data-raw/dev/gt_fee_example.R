search_fee_schedule(
  hcpcs    = "39503",
  state    = "GA",
  locality = "99",
  mac      = "10212") |>
  fuimus::remove_quiet() |>
  rowwise() |>
  mutate(cpt_desc_clin = map(cpt_desc_clin, ~paste0("* ", ., collapse = "\n"))) |>
  unnest(cols = cpt_desc_clin) |>
  ungroup() |>
  case_multproc(mult_proc) |>
  case_global(global) |>
  case_cosurg(surg_co) |>
  case_assistant(surg_asst) |>
  case_status(status) |>
  case_supervision(supvis) |>
  case_bilateral(surg_bilat) |>
  case_team(surg_team) |>
  case_imaging(dximg) |>
  case_pctc(pctc) |>
  case_opps(opps) |>
  case_chapter_cpt(hcpcs) |>
  case_category(hcpcs) |>
  select("CPT Code"                     = hcpcs,
         Description                    = description,
         Consumer                       = cpt_desc_cons,
         Clinician                      = cpt_desc_clin,
         "CPT Chapter"                  = cpt_chapter,
         "CPT Range"                    = cpt_range,
         "CPT Category"                 = category,
         "Fee Status"                   = status,
         "RBCS Category"                = rbcs_category,
         "RBCS Family"                  = rbcs_family,
         MAC                            = mac,
         State                          = state,
         Locality                       = locality,
         Area                           = area,
         Counties                       = counties,
         "Work GPCI"                    = wgpci,
         "Practice GPCI"                = pgpci,
         "Malpractice GPCI"             = mgpci,
         "Work RVU"                     = wrvu,
         "Non-Facility Practice RVU"    = nfprvu,
         "Facility Practice RVU"        = fprvu,
         "Malpractice RVU"              = mrvu,
         "Conversion Factor"            = cf,
         "Total Facility RVUs"          = frvus,
         "Total Non-Facility RVUs"      = nrvus,
         "Facility PAR Fee"             = fpar,
         "Non-Facility PAR Fee"         = npar,
         "Facility NON-PAR Fee"         = fnpar,
         "Non-Facility NON-PAR Fee"     = nfnpar,
         "Facility Limiting Charge"     = flim,
         "Non-Facility Limiting Charge" = nlim,
         "Multiple Procedures"          = mproc_description,
         "Global Days"                  = global_description,
         "Preoperative"                 = op_pre,
         "Intraoperative"               = op_intra,
         "Postoperative"                = op_post,
         "Assistant at Surgery"         = asst_description,
         "Co-Surgeons"                  = cosurg_description,
         "Team Surgery"                 = team_description,
  ) |>
  display_long() |>
  gt(groupname_col         = "name",
     row_group_as_column   = TRUE) |>
  gt_style(tablign         = "right",
           tabsize         = 14,
           tabwt           = "bold") |>
  sub_missing(missing_text = "Concept Does Not Apply.") |>
  fmt_markdown() |>
  tab_style(
    style                = cell_text(
      font                 = google_font(
        name                 = "Fira Code")),
    locations            = cells_body(
      columns            = contains("value"))) |>
  tab_style(
    style = list(
      cell_fill(color = "powderblue"),
      cell_text(weight = "bold"),
      cell_borders(
        sides = c("all"),
        color = "powderblue",
        weight = px(3))
    ),
    locations = cells_row_groups())
