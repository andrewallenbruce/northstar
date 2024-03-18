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
#' non_participating_amount(26.35)
#' @export
non_participating_amount <- function(par_amount) {

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
    nonpar = non_participating_amount(fpar),
    limit  = limiting_charge(fpar))

  n <- list(
    prvu   = nprvu,
    rvu    = nrvus,
    par    = npar,
    nonpar = non_participating_amount(npar),
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
}
