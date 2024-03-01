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
#' calc_limiting_charge(26.35)
#' @export
calc_nonpar_amount <- function(participating_amount) {

  stopifnot("`participating_amount` must be numeric" = is.numeric(participating_amount))

  participating_amount * 0.95
}

#' Calculate Physician Fee Schedule Payment Amounts
#' @param wrvu numeric
#' @param prvu_f numeric
#' @param prvu_nf numeric
#' @param mrvu numeric
#' @param wgpci numeric
#' @param pgpci numeric
#' @param mgpci numeric
#' @param cf numeric
#' @return description
#' @examples
#' calc_amounts(wrvu     = 6.26,
#'              prvu_nf  = 7.92,
#'              prvu_f   = 4.36,
#'              mrvu     = 0.99,
#'              wgpci    = 1.0,
#'              pgpci    = 0.883,
#'              mgpci    = 1.125,
#'              cf       = 32.7442)
#' @export
calc_amounts <- function(wrvu,
                         prvu_f,
                         prvu_nf,
                         mrvu,
                         wgpci,
                         pgpci,
                         mgpci,
                         cf) {

  stopifnot("all arguments must be numeric" = is.numeric(
    c(wrvu, prvu_f, prvu_nf, mrvu, wgpci, pgpci, mgpci, cf)))

  par_amt_f  <- ((wrvu * wgpci) + (prvu_f * pgpci) + (mrvu * mgpci)) * cf
  par_amt_nf <- ((wrvu * wgpci) + (prvu_nf * pgpci) + (mrvu * mgpci)) * cf

  f <- list(
    par    = par_amt_f,
    nonpar = calc_nonpar_amount(par_amt_f),
    limit  = calc_limiting_charge(par_amt_f))

  nf <- list(
    par    = par_amt_nf,
    nonpar = calc_nonpar_amount(par_amt_nf),
    limit  = calc_limiting_charge(par_amt_nf))

  glue::glue("Facility:\n",
             "Participating Amount    = {gt::vec_fmt_currency(parf)}\n",
             "Non-Particpating Amount = {gt::vec_fmt_currency(nonparf)}\n",
             "Limiting Charge         = {gt::vec_fmt_currency(limitf)}",
             "\n\n",
             "Non-Facility:\n",
             "Participating Amount    = {gt::vec_fmt_currency(parnf)}\n",
             "Non-Particpating Amount = {gt::vec_fmt_currency(nonparnf)}\n",
             "Limiting Charge         = {gt::vec_fmt_currency(limitnf)}",
             parf     = f$par,
             nonparf  = f$nonpar,
             limitf   = f$limit,
             parnf    = nf$par,
             nonparnf = nf$nonpar,
             limitnf  = nf$limit)
}
