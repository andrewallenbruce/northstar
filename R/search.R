#' Look up Information about HCPCS Codes
#' @param hcpcs < *character* > 5-character HCPCS Code
#' @param state < *character* > 2-character state abbreviation
#' @param locality < *character* > 2-character locality id
#' @param mac < *character* > 5-character MAC id code
#'
#' @return A [tibble][tibble::tibble-package] with the columns:
#'
#' |**Column**    |**Description**                       |
#' |:-------------|:-------------------------------------|
#' |`hcpcs`       |5-character HCPCS Code                |
#' |`hcpcs_type`  |HCPCS Type                            |
#' |`description` |HCPCS Description                     |
#' |`cons_desc`   |HCPCS Level I Consumer Description    |
#' |`clin_descs`  |HCPCS Level I Clinician Descriptions  |
#' |`mod`         |Modifier                              |
#' |`status`      |Status Code                           |
#' |`mac`         |Medicare Administrative Contractor ID |
#' |`state`       |State Abbreviation                    |
#' |`locality`    |State Abbreviation                    |
#' |`area`        |State Abbreviation                    |
#' |`counties`    |State Abbreviation                    |
#' |`two_macs`    |State Abbreviation                    |
#' |`wgpci`       |State Abbreviation                    |
#' |`pgpci`       |State Abbreviation                    |
#'
#' @examplesIf interactive()
#' hcpcs_search(hcpcs = "V5299",
#'              state = "GA",
#'              locality = "99",
#'              mac = "10212")
#'
#' hcpcs_search(hcpcs = c("39503", "43116", "33935", "11646"), state = "GA")
#' @autoglobal
#' @export
hcpcs_search <- function(hcpcs,
                         state = NULL,
                         locality = NULL,
                         mac = NULL) {

  rlang::check_required(hcpcs)

  # rv <- purrr::map(hcpcs, \(x) rvu(hcpcs = x)) |> purrr::list_rbind()

  rv <- rvu(hcpcs     = hcpcs)

  if (vctrs::vec_is_empty(rv)) {
    cli::cli_abort(
      "No RVUs found for HCPCS code {.strong {.val {hcpcs}}}."
    )
  }

  gp <- gpci(state    = state,
             locality = locality,
             mac      = mac)

  fs <- pfs(hcpcs     = hcpcs,
            locality  = locality,
            mac       = mac)


  ds <- descriptors(hcpcs = hcpcs) |>
    tidyr::nest(clinician_descriptors = clinician_descriptor)

  l2 <- level2(hcpcs = hcpcs)

  rb <- rbcs(hcpcs = hcpcs) |>
    dplyr::select(hcpcs,
                  rbcs_cat = category,
                  rbcs_sub = subcategory,
                  rbcs_fam = family,
                  rbcs_pro = procedure)

  x <- list(
    rvus        = if (!vctrs::vec_is_empty(rv)) rv else NULL,
    gpci        = if (!vctrs::vec_is_empty(gp)) gp else NULL,
    payment     = if (!vctrs::vec_is_empty(fs)) fs else NULL,
    descriptors = if (!vctrs::vec_is_empty(ds)) ds else NULL,
    level_2     = if (!vctrs::vec_is_empty(l2)) l2 else NULL,
    rbcs        = if (!vctrs::vec_is_empty(rb)) rb else NULL) |>
    purrr::compact()


  res <- dplyr::cross_join(x$rvus,
                           x$gpci) |>
    dplyr::left_join(x$rbcs,
                     by = dplyr::join_by(hcpcs))

  if (rlang::has_name(x, "level_2")) {

    res <- dplyr::left_join(res, x$level_2, by = dplyr::join_by(hcpcs)) |>
      case_section_hcpcs(hcpcs)

  }

  if (rlang::has_name(x, "descriptors")) {

    res <- dplyr::left_join(res,x$payment,
           by = dplyr::join_by(hcpcs, mod, status, mac, locality)) |>
      dplyr::left_join(x$descriptors,
           by = dplyr::join_by(hcpcs == cpt)) |>
      case_category(hcpcs) |>
      case_section_cpt(hcpcs)

  }

  res |>
    dplyr::mutate(
      fpar  = ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci)) * cf,
      npar  = ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci)) * cf,
      fnpar = nonpar_amount(fpar),
      nnpar = nonpar_amount(npar),
      flim  = limiting_charge(fpar),
      nlim  = limiting_charge(npar)) |>
    case_level(hcpcs) |>
    cols_amounts()

}

#' @param df data frame
#' @autoglobal
#' @noRd
cols_amounts <- function(df) {

  cols <- c('hcpcs',
            'hcpcs_type',
            'level',
            'category',
            'section',
            'rbcs_cat',
            'rbcs_sub',
            'rbcs_fam',
            'rbcs_pro',
            'description_rvu' = 'description',
            'description_long' = 'long_description',
            'description_short' = 'short_description',
            'description_consumer' = 'consumer_descriptor',
            'description_clinician' = 'clinician_descriptors',
            'mod',
            'status',
            'mac',
            'state',
            'locality',
            'area' = 'name',
            # 'counties',
            # 'two_macs',
            'wgpci',
            'pgpci',
            'mgpci',
            'wrvu',
            'nonfac_prvu' = 'nprvu',
            'fac_prvu' = 'fprvu',
            'mrvu',
            'cf',
            'fac_par' = 'fpar',
            'nonfac_par' = 'npar',
            'fac_nonpar' = 'fnpar',
            'nonfac_nonpar' = 'nnpar',
            'fac_limit' = 'flim',
            'nonfac_limit' = 'nlim',
            'opps',
            'nonfac_prvu_opps' = 'nprvu_opps',
            'fac_prvu_opps' = 'fprvu_opps',
            'mrvu_opps',
            'fac_par_opps' = 'fpar_opps',
            'nonfac_par_opps' = 'npar_opps',
            'fac_nonpar_opps' = 'fnpar_opps',
            'nonfac_nonpar_opps' = 'nnpar_opps',
            'fac_limit_opps' = 'flim_opps',
            'nonfac_limit_opps' = 'nlim_opps',
            'mult_surg',
            'mult_proc',
            'flat_vis',
            'nonfac_therapy' = 'nther',
            'fac_therapy' = 'fther',
            'global',
            'op_ind',
            'op_pre',
            'op_intra',
            'op_post',
            'pctc',
            'surg_bilat',
            'surg_asst',
            'surg_co',
            'surg_team',
            'supvis',
            'dximg',
            'endo',
            'rare',
            'unused',
            'price',
            'mult_pi',
            'cim',
            'mcm',
            'statute',
            'labcert',
            'xref',
            'cov',
            'asc_grp',
            'asc_dt',
            'procnote',
            'betos',
            'tos'
  )
  df |> dplyr::select(dplyr::any_of(cols))
}
