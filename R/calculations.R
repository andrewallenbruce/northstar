#' Calculate Limiting Charge
#' @param participating_amount numeric
#' @return description
#' @examples
#' calc_limiting_charge(26.35)
#' @export
calc_limiting_charge <- function(participating_amount) {

  stopifnot("`participating_amount` must be numeric" = is.numeric(participating_amount))

  participating_amount * 1.0925
}

#' Calculate Non-Participating Amount
#' @param participating_amount numeric
#' @return description
#' @examples
#' calc_nonpar_amount(26.35)
#' @export
calc_nonpar_amount <- function(participating_amount) {

  stopifnot("`participating_amount` must be numeric" = is.numeric(participating_amount))

  participating_amount * 0.95
}

#' Calculate Physician Fee Schedule Payment Amounts
#'
#' ((Work RVU x Work GPCI) + (PE RVU x PE GPCI) + (MP RVU x MP GPCI)) x CF
#'
#' @param wrvu Work RVU
#' @param fprvu Facility Practice Expense RVU
#' @param nprvu Non-Facility Practice Expense RVU
#' @param mrvu Malpractice RVU
#' @param wgpci Work GPCI
#' @param pgpci Practice Expense GPCI
#' @param mgpci Malpractice GPCI
#' @param cf Conversion Factor
#' @return Facility & Non-Facility Participating, Non-Participating & Limiting Charge Amounts
#' @examples
#' calc_amounts(wrvu  = 6.26,
#'              nprvu = 7.92,
#'              fprvu = 4.36,
#'              mrvu  = 0.99,
#'              wgpci = 1.0,
#'              pgpci = 0.883,
#'              mgpci = 1.125,
#'              cf    = 32.7442)
#' @autoglobal
#' @export
calc_amounts <- function(wrvu,
                         fprvu,
                         nprvu,
                         mrvu,
                         wgpci,
                         pgpci,
                         mgpci,
                         cf) {

  stopifnot("all arguments must be numeric" = is.numeric(
    c(wrvu, fprvu, nprvu, mrvu, wgpci, pgpci, mgpci, cf)))

  fpar <- ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci)) * cf
  npar <- ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci)) * cf

  f <- list(
    par    = fpar,
    nonpar = calc_nonpar_amount(fpar),
    limit  = calc_limiting_charge(fpar))

  n <- list(
    par    = npar,
    nonpar = calc_nonpar_amount(npar),
    limit  = calc_limiting_charge(npar))

  glue::glue("Facility:\n",
             "Participating Amount    = {gt::vec_fmt_currency(fpar)}\n",
             "Non-Particpating Amount = {gt::vec_fmt_currency(fnpar)}\n",
             "Limiting Charge         = {gt::vec_fmt_currency(flim)}",
             "\n\n",
             "Non-Facility:\n",
             "Participating Amount    = {gt::vec_fmt_currency(npar)}\n",
             "Non-Particpating Amount = {gt::vec_fmt_currency(nnpar)}\n",
             "Limiting Charge         = {gt::vec_fmt_currency(nlim)}",
             fpar  = f$par,
             fnpar = f$nonpar,
             flim  = f$limit,
             npar  = n$par,
             nnpar = n$nonpar,
             nlim  = n$limit)
}

#' Calculate Physician Fee Schedule Payment Amounts
#' @param hcpcs numeric
#' @param state numeric
#' @param locality numeric
#' @param mac numeric
#' @return description
#' @examplesIf interactive()
#' calc_amounts_df(hcpcs = "11646",
#'                 state = "GA",
#'                 locality = "99",
#'                 mac = "10212")
#'
#' calc_amounts_df(hcpcs = c("39503", "43116", "33935", "11646"),
#'                 state = "GA")
#' @autoglobal
#' @export
calc_amounts_df <- function(hcpcs,
                            state = NULL,
                            locality = NULL,
                            mac = NULL) {

  cf <- 32.7442

  rv <- purrr::map(hcpcs, \(x) rvu(hcpcs = x)) |>
    purrr::list_rbind()

  gp <- gpci(state = state,
             locality = locality,
             mac = mac)

  fs <- purrr::map(hcpcs, \(x) pfs(hcpcs = x,
                                   locality = locality,
                                   mac = mac)) |>
    purrr::list_rbind()

  res <- dplyr::left_join(gp, fs, by = dplyr::join_by(mac, locality)) |>
    dplyr::left_join(rv, by = dplyr::join_by(hcpcs, mod, status)) |>
    dplyr::mutate(
      fpar  = ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci)) * cf,
      npar  = ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci)) * cf,
      fnpar = calc_nonpar_amount(fpar),
      nnpar = calc_nonpar_amount(npar),
      flim  = calc_limiting_charge(fpar),
      nlim  = calc_limiting_charge(npar))

  dplyr::select(res,
                hcpcs,
                description,
                mod,
                status,
                mac,
                state,
                locality,
                area = name,
                counties,
                two_macs,
                wgpci,
                pgpci,
                mgpci,
                wrvu,
                nprvu,
                fprvu,
                mrvu,
                cf,
                fpar,
                npar,
                fnpar,
                nnpar,
                flim,
                nlim,
                opps,
                nprvu_opps,
                mrvu_opps,
                fprvu_opps,
                mult_surg,
                flat_vis,
                ntherapy,
                ftherapy,
                global,
                op_ind,
                op_pre,
                op_intra,
                op_post,
                pctc,
                mult_proc,
                surg_bilat,
                surg_asst,
                surg_co,
                surg_team,
                supvis,
                dximg,
                endo,
                rare,
                unused
                )

}
