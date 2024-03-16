#' Calculate Limiting Charge
#'
#' The Medicare limiting charge is set by law at 115% of the payment amount
#' for the service furnished by the Non-participating physician.
#'
#' However, the law sets the payment amount for Non-participating physicians
#' at 95% of the payment amount for Participating physicians (i.e., the fee
#' schedule amount).
#'
#' Calculating 95% of 115% of an amount is equivalent to multiplying the
#' amount by a factor of 1.0925 (or 109.25%).
#'
#' Therefore, to calculate the Medicare limiting charge for a physician
#' service for a locality, multiply the fee schedule amount by a factor
#' of 1.0925. The result is the Medicare limiting charge for that service for
#' that locality to which the fee schedule amount applies.
#'
#' @param par_amount < *double* > Participating Amount
#' @return numeric vector of Limiting Charge Amount
#' @examples
#' limiting_charge(26.35)
#' @export
limiting_charge <- function(par_amount) {

  stopifnot("`par_amount` must be numeric" = is.numeric(par_amount))

  par_amount * 1.0925
}

#' Calculate Non-Participating Amount
#'
#' The payment amount for Non-participating physicians is 95% of the payment
#' amount for Participating physicians (i.e., the fee schedule amount).
#'
#' @param par_amount < *double* > Participating Amount
#' @return numeric vector of Non-participating amount
#' @examples
#' nonpar_amount(26.35)
#' @export
nonpar_amount <- function(par_amount) {

  stopifnot("`par_amount` must be numeric" = is.numeric(par_amount))

  par_amount * 0.95
}

#' Calculate Physician Fee Schedule Payment Amounts
#'
#' ((wRVU x wGPCI) + (pRVU x pGPCI) + (mRVU x mGPCI)) x Conversion Factor
#'
#' @param wrvu < *double* > Work RVU
#' @param fprvu < *double* > Facility Practice Expense RVU
#' @param nprvu < *double* > Non-Facility Practice Expense RVU
#' @param mrvu < *double* > Malpractice RVU
#' @param wgpci < *double* > Work GPCI
#' @param pgpci < *double* > Practice Expense GPCI
#' @param mgpci < *double* > Malpractice GPCI
#' @param cf < *double* > Conversion Factor, default is `32.7442`
#' @return **Facility** & **Non-Facility** Participating, Non-Participating & Limiting Charge Amounts
#' @examples
#' calculate_amounts(wrvu  = 6.26,
#'                   nprvu = 7.92,
#'                   fprvu = 4.36,
#'                   mrvu  = 0.99,
#'                   wgpci = 1.053,
#'                   pgpci = 0.883,
#'                   mgpci = 1.125,
#'                   cf    = 32.7442)
#' @autoglobal
#' @export
calculate_amounts <- function(wrvu,
                              fprvu,
                              nprvu,
                              mrvu,
                              wgpci,
                              pgpci,
                              mgpci,
                              cf = 32.7442) {

  stopifnot("all arguments must be numeric" = is.numeric(
    c(wrvu, fprvu, nprvu, mrvu, wgpci, pgpci, mgpci, cf)))

  frvus <- ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci))
  nrvus <- ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci))

  fpar <- frvus * cf
  npar <- nrvus * cf

  f <- list(
    prvu   = fprvu,
    rvu    = frvus,
    par    = fpar,
    nonpar = nonpar_amount(fpar),
    limit  = limiting_charge(fpar))

  n <- list(
    prvu   = nprvu,
    rvu    = nrvus,
    par    = npar,
    nonpar = nonpar_amount(npar),
    limit  = limiting_charge(npar))

  cli::cli_inform(c(
    "{.strong {.emph Facility}} Amounts:",
    "\n",
    "RVU Total ............ {.strong {.val {rlang::sym(gt::vec_fmt_number(f$rvu))}}}",
    "Participating ........ {.strong {.val {rlang::sym(gt::vec_fmt_currency(f$par))}}}",
    "Non-Particpating ..... {.strong {.val {rlang::sym(gt::vec_fmt_currency(f$nonpar))}}}",
    "Limiting Charge ...... {.strong {.val {rlang::sym(gt::vec_fmt_currency(f$limit))}}}",
    "\n\n",

    "{.strong {.emph Non-Facility}} Amounts:",
    "\n",
    "RVU Total ............ {.strong {.val {rlang::sym(gt::vec_fmt_number(n$rvu))}}}",
    "Participating ........ {.strong {.val {rlang::sym(gt::vec_fmt_currency(n$par))}}}",
    "Non-Particpating ..... {.strong {.val {rlang::sym(gt::vec_fmt_currency(n$nonpar))}}}",
    "Limiting Charge ...... {.strong {.val {rlang::sym(gt::vec_fmt_currency(n$limit))}}}"
    )
  )
  invisible(list(fac = f, non = n))

  # glue::glue("Facility:\n",
  #            "Participating Amount    = {gt::vec_fmt_currency(fpar)}\n",
  #            "Non-Particpating Amount = {gt::vec_fmt_currency(fnpar)}\n",
  #            "Limiting Charge         = {gt::vec_fmt_currency(flim)}",
  #            "\n\n",
  #            "Non-Facility:\n",
  #            "Participating Amount    = {gt::vec_fmt_currency(npar)}\n",
  #            "Non-Particpating Amount = {gt::vec_fmt_currency(nnpar)}\n",
  #            "Limiting Charge         = {gt::vec_fmt_currency(nlim)}",
  #            fpar  = f$par,
  #            fnpar = f$nonpar,
  #            flim  = f$limit,
  #            npar  = n$par,
  #            nnpar = n$nonpar,
  #            nlim  = n$limit)
}

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
#' hcpcs_search(hcpcs = c("39503", "43116", "33935", "11646"),
#'              state = "GA")
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

  rb <- rbcs(hcpcs = hcpcs)

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

    res <- dplyr::left_join(res,
                            x$level_2,
                     by = dplyr::join_by(hcpcs))

  }

  if (rlang::has_name(x, "descriptors")) {

    res <- dplyr::left_join(res,
                            x$payment,
           by = dplyr::join_by(hcpcs,
                               mod,
                               status,
                               mac,
                               locality)) |>
      dplyr::left_join(x$descriptors,
           by = dplyr::join_by(hcpcs == cpt))

  }

  res |>
    dplyr::mutate(
    fpar  = ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci)) * cf,
    npar  = ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci)) * cf,
    fnpar = nonpar_amount(fpar),
    nnpar = nonpar_amount(npar),
    flim  = limiting_charge(fpar),
    nlim  = limiting_charge(npar)) |>
    cols_amounts() |>
    case_category(hcpcs) |>
    case_level(hcpcs) |>
    case_section_cpt(hcpcs) |>
    case_section_hcpcs(hcpcs)

    # fpar_opps = ((wrvu * wgpci) + (fprvu_opps * pgpci) + (mrvu_opps * mgpci)) * cf,
    # npar_opps  = ((wrvu * wgpci) + (nprvu_opps * pgpci) + (mrvu_opps * mgpci)) * cf,
    # fnpar_opps = nonpar_amount(fpar_opps),
    # nnpar_opps = nonpar_amount(npar_opps),
    # flim_opps  = limiting_charge(fpar_opps),
    # nlim_opps  = limiting_charge(npar_opps)

}

#' @param df data frame
#' @autoglobal
#' @noRd
cols_amounts <- function(df) {

  cols <- c('hcpcs',
            'hcpcs_type',
            'description',
            'long_description',
            'short_description',
            'clin_desc' = 'clinician_descriptor',
            'cons_desc' = 'consumer_descriptor',
            'clin_descs' = 'clinician_descriptors',
            'mod',
            'status',
            'mac',
            'state',
            'locality',
            'area' = 'name',
            'counties',
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
            'tos',
            'category',
            'subcategory',
            'family',
            'procedure'
            # 'date_added',
            # 'date_action',
            # 'date_ended',
            # 'action',
            # 'rbcs',
            # 'cat.id',
            # 'sub.id',
            # 'fam.id',
            # 'date_hcpcs_add',
            # 'date_hcpcs_end',
            # 'date_rbcs_assign'
            )

  df |> dplyr::select(dplyr::any_of(cols))
}
