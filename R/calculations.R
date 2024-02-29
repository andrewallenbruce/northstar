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

  participating_amount * 1.0925
}

#' Calculate Physician Fee Schedule Payment Amounts
#' @param wrvu numeric
#' @param prvu numeric
#' @param mrvu numeric
#' @param wgpci numeric
#' @param pgpci numeric
#' @param mgpci numeric
#' @param cf numeric
#' @return description
#' @examples
#' calc_amounts(wrvu  = 108.91,
#'              prvu  = 35.14,
#'              mrvu  = 26.95,
#'              wgpci = 1.0,
#'              pgpci = 0.997,
#'              mgpci = 1.128,
#'              cf    = 32.7442)
#' @export
calc_amounts <- function(wrvu,
                         prvu,
                         mrvu,
                         wgpci,
                         pgpci,
                         mgpci,
                         cf) {

  stopifnot("all arguments must be numeric" = is.numeric(
    c(wrvu, prvu, mrvu, wgpci, pgpci, mgpci, cf)))

  par_amt <- ((wrvu * wgpci) + (prvu * pgpci) + (mrvu * mgpci)) * cf

  x <- list(
    par    = par_amt,
    nonpar = calc_nonpar_amount(par_amt),
    limit  = calc_limiting_charge(par_amt))

  glue::glue("Participating Amount:    {gt::vec_fmt_currency(x$par)}\n",
             "Non-Particpating Amount: {gt::vec_fmt_currency(x$nonpar)}\n",
             "Limiting Charge:         {gt::vec_fmt_currency(x$limit)}",
             par = x$par,
             nonpar = x$nonpar,
             limit = x$limit)
}
