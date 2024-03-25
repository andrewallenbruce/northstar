#' Look up Information about HCPCS Codes
#' @param hcpcs < *character* > 5-character HCPCS Code
#' @param state < *character* > 2-character state abbreviation
#' @param locality < *character* > 2-character locality id
#' @param mac < *character* > 5-character MAC id code
#' @param ... description
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
#' hcpcs_search(hcpcs = c("39503", "43116", "33935", "11646", "70170"), state = "GA")
#' @autoglobal
#' @export
hcpcs_search <- function(hcpcs,
                         state = NULL,
                         locality = NULL,
                         mac = NULL,
                         ...) {

  rlang::check_required(hcpcs)

  rv <- rvu(hcpcs = hcpcs)

  if (vctrs::vec_is_empty(rv)) {
    cli::cli_abort("HCPCS code {.strong {.val {hcpcs}}} not found.")}

  gp <- gpci(state = state, locality = locality, mac = mac)
  fs <- pfs(hcpcs = hcpcs, locality = locality, mac = mac)
  op <- opps(hcpcs = hcpcs, locality = locality, mac = mac)
  ds <- descriptors(hcpcs = hcpcs)
  l2 <- level2(hcpcs = hcpcs)
  rb <- rbcs(hcpcs = hcpcs)

  x <- list(
    rvus        = if (!vctrs::vec_is_empty(rv)) rv else NULL,
    gpci        = if (!vctrs::vec_is_empty(gp)) gp else NULL,
    payment     = if (!vctrs::vec_is_empty(fs)) fs else NULL,
    oppscap     = if (!vctrs::vec_is_empty(op)) op else NULL,
    descriptors = if (!vctrs::vec_is_empty(ds)) ds else NULL,
    level_2     = if (!vctrs::vec_is_empty(l2)) l2 else NULL,
    rbcs        = if (!vctrs::vec_is_empty(rb)) rb else NULL) |>
    purrr::compact()


  res <- dplyr::cross_join(x$rvus, x$gpci) |>
         dplyr::left_join(x$rbcs, by = dplyr::join_by(hcpcs))

  if (all(rlang::has_name(x, c("level_2", "descriptors")))) {

    res <- dplyr::left_join(res, x$level_2,
           by = dplyr::join_by(hcpcs)) |>
           dplyr::left_join(x$payment,
           by = dplyr::join_by(hcpcs, mod, status, pctc, mac, locality)) |>
           dplyr::left_join(x$descriptors,
           by = dplyr::join_by(hcpcs))

  }

  if (rlang::has_name(x, "level_2") & !rlang::has_name(x, "descriptors")) {

    res <- dplyr::left_join(res, x$level_2, by = dplyr::join_by(hcpcs))

  }

  if (rlang::has_name(x, "descriptors") & !rlang::has_name(x, "level_2")) {

    res <- dplyr::left_join(res,x$payment,
           by = dplyr::join_by(hcpcs, mod, status, pctc, mac, locality)) |>
           dplyr::left_join(x$descriptors,
           by = dplyr::join_by(hcpcs))

  }

  if (rlang::has_name(x, "oppscap")) {

    res <- dplyr::left_join(res, x$oppscap,
           by = dplyr::join_by(hcpcs, mod, status, mac, locality))
 }

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
            'tos'
  )
  df |> dplyr::select(dplyr::any_of(cols), dplyr::everything(

  ))
}

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
