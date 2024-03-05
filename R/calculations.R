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
#' @param wrvu Work RVUs
#' @param fprvu Facility Practice Expense RVUs
#' @param nprvu Non-Facility Practice Expense RVUs
#' @param mrvu Malpractice RVUs
#' @param wgpci Work GPCI
#' @param pgpci Practice Expense GPCI
#' @param mgpci Malpractice GPCI
#' @param cf Conversion Factor
#' @return Facility & Non-Facility Participating, Non-Participating & Limiting Charge Amounts
#' @examples
#' calc_amounts(wrvu = 6.26,
#'              nprvu = 7.92,
#'              fprvu = 4.36,
#'              mrvu = 0.99,
#'              wgpci = 1.0,
#'              pgpci = 0.883,
#'              mgpci = 1.125,
#'              cf = 32.7442)
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
#' @autoglobal
#' @export
calc_amounts_df <- function(hcpcs, state = NULL, locality = NULL, mac = NULL) {

  cf <- 32.7442

  rvus <- rvu(hcpcs = hcpcs)

  gp <- gpci(
    state    = state,
    locality = locality,
    mac      = mac)

  fs <- pfs(
    hcpcs    = hcpcs,
    locality = locality,
    mac      = mac)

  vctrs::vec_cbind(rvus, gp) |>
    dplyr::left_join(fs, by = dplyr::join_by(hcpcs, mod, status, mac, locality)) |>
    dplyr::mutate(
      fpar  = ((wrvu * wgpci) + (fprvu * pgpci) + (mrvu * mgpci)) * cf,
      npar  = ((wrvu * wgpci) + (nprvu * pgpci) + (mrvu * mgpci)) * cf,
      fnpar = calc_nonpar_amount(fpar),
      nnpar = calc_nonpar_amount(npar),
      flim  = calc_limiting_charge(fpar),
      nlim  = calc_limiting_charge(npar))

}
