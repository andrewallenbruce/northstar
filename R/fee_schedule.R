#' Search Physician Fee Schedule
#'
#' @param hcpcs < *chr* > 5-character HCPCS Code
#' @param state < *chr* > 2-character State Abbreviation
#' @param locality < *chr* > 2-digit Locality ID
#' @param mac < *chr* > 5-digit MAC ID code
#' @param ... Empty
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
#' @examples
#' search_fee_schedule(hcpcs    = c("V5299", "70170"),
#'                     state    = "GA",
#'                     locality = "99",
#'                     mac      = "10212")
#' @autoglobal
#' @export
search_fee_schedule <- function(hcpcs,
                                state    = NULL,
                                locality = NULL,
                                mac      = NULL,
                                ...) {

  rlang::check_required(hcpcs)

  # retrieve Relative Value Units
  rv <- search_rvu(hcpcs = hcpcs)

  # if no data found in rvu file, nothing will be found in others
  msg <- "HCPCS code {.strong {.val {hcpcs}}} not found."
  if (vctrs::vec_is_empty(rv)) {cli::cli_abort(msg)}

  # test if all are NULL
  # test <- !vctrs::vec_is_empty(c(state, locality, mac))

  # if all are NULL, don't call gpci
  # gp <- switch(
  # test,
  # "TRUE" = gpci(state = state, locality = locality, mac = mac),
  # "FALSE" = character(0))

  # retrieve Geographic Practice Cost Indices
  gp <- search_gpci(state = state, locality = locality, mac = mac)

  # retrieve Payment Amounts
  fs <- search_payment(hcpcs = hcpcs, locality = locality, mac = mac)

  # retrieve OPPS Capitations
  op <- search_opps(hcpcs = hcpcs, locality = locality, mac = mac)

  # retrieve CPT codes
  ds <- search_cpt(hcpcs = hcpcs)

  # retrieve Level II HCPCS codes
  l2 <- search_hcpcs(hcpcs = hcpcs)

  # retrieve Restructured BETOS Classifications
  rb <- search_rbcs(hcpcs = hcpcs)

  # put results in list, remove NULLs
  x <- list(
    rvu = if (!vctrs::vec_is_empty(rv)) rv else NULL,
    gpc = if (!vctrs::vec_is_empty(gp)) gp else NULL,
    pay = if (!vctrs::vec_is_empty(fs)) fs else NULL,
    opp = if (!vctrs::vec_is_empty(op)) op else NULL,
    cpt = if (!vctrs::vec_is_empty(ds)) ds else NULL,
    lvl = if (!vctrs::vec_is_empty(l2)) l2 else NULL,
    rbc = if (!vctrs::vec_is_empty(rb)) rb else NULL) |>
    purrr::compact()

  # create join_by objects
  byhcpc <- dplyr::join_by(hcpcs)
  bypctc <- dplyr::join_by(hcpcs, mod, status, pctc, mac, locality)
  nopctc <- dplyr::join_by(hcpcs, mod, status, mac, locality)

  # cross join rvu and gpci, left join rbcs
  res <- dplyr::cross_join(x$rvu, x$gpc) |>
         dplyr::left_join(x$rbc, byhcpc)

  # test if results contain hcpcs and cpts
  both <- all(rlang::has_name(x, c("lvl", "cpt")))

  # test if results contain hcpcs only
  lvl2 <- rlang::has_name(x, "lvl") & !rlang::has_name(x, "cpt")

  # test if results contain cpts only
  lvl1 <- rlang::has_name(x, "cpt") & !rlang::has_name(x, "lvl")

  # only one should be true, extract its name
  path <- list(
    both = if (both) both else NULL,
    lvl2 = if (lvl2) lvl2 else NULL,
    lvl1 = if (lvl1) lvl1 else NULL) |>
    purrr::compact() |>
    names()

  # perform join based on path
  res <- switch(path,
    "both" = dplyr::left_join(res, x$lvl, byhcpc) |>
             dplyr::left_join(x$pay, bypctc) |>
             dplyr::left_join(x$cpt, byhcpc),
    "lvl2" = dplyr::left_join(res, x$lvl, byhcpc),
    "lvl1" = dplyr::left_join(res, x$pay, bypctc) |>
             dplyr::left_join(x$cpt, byhcpc))

  # if opps data is available, left join
  if (rlang::has_name(x, "opp")) {res <- dplyr::left_join(res, x$opp, nopctc)}

  res |>
    dplyr::mutate(
      frvus  = sum(wrvu * wgpci, fprvu * pgpci, mrvu * mgpci),
      nrvus  = sum(wrvu * wgpci, nfprvu * pgpci, mrvu * mgpci),
      fpar   = frvus * 32.7442,
      npar   = nrvus * 32.7442,
      fnpar  = fpar * 0.95,
      nfnpar = npar * 0.95,
      flim   = fpar * 1.0925,
      nlim   = npar * 1.0925) |>
    cols_amounts()
}

#' @param df data frame
#' @autoglobal
#' @noRd
cols_amounts <- function(df) {

  cols <- c('hcpcs',
            'description',
            'description_long',
            'description_consumer',
            'descriptions_clinician',
            'rbcs_category',
            'rbcs_subcategory',
            'rbcs_family',
            'rbcs_procedure',
            'status',
            'mac',
            'state',
            'locality',
            'area',
            'counties',
            'wgpci',
            'pgpci',
            'mgpci',
            'wrvu',
            'nprvu',
            'fprvu',
            'mrvu',
            'cf',
            'f_fee',
            'nf_fee',
            'frvus',
            'nrvus',
            'fpar',
            'npar',
            'fnpar',
            'nfnpar',
            'flim',
            'nlim',
            'opps',
            'opps_nf',
            'opps_f',
            'nprvu_opps',
            'fprvu_opps',
            'mrvu_opps',
            'fpymt_opps',
            'nfpymt_opps',
            'mult_surg',
            'mult_proc',
            'nther',
            'fther',
            'global',
            'op_ind',
            'op_pre',
            'op_intra',
            'op_post',
            'mod',
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
            'mult',
            'labcert',
            'xref',
            'coverage',
            'asc',
            'betos',
            'tos')
  df |>
    dplyr::select(
      dplyr::any_of(cols),
      dplyr::everything())
}

# search_hcpcs(hcpcs = c("39503", "43116", "33935", "11646", "70170"), state = "GA")
# rv <- purrr::map(hcpcs, \(x) rvu(hcpcs = x)) |> purrr::list_rbind()
#
# # nppes_pmap <- function(...) {
#   provider::nppes(state = "KS", ...)
# }
#
# names <- list(first = first_last$first,
#               last = first_last$last)
#
# npi <- pmap(names, nppes_pmap) |>
#   list_rbind()
