#' Calculate limiting charge
#' @param x numeric
#' @return description
#' @examples
#' limiting_charge(26.35)
#' @export
limiting_charge <- function(x) {
  stopifnot("x must be numeric" = is.numeric(x))
  x * 1.0925     # PAR FEE
  # x * 1.150115 # NON-PAR FEE
}

#' @noRd
calc_payment <- function(wk.rvu,
                         wk.gpci,
                         pe.rvu,
                         pe.gpci,
                         mp.rvu,
                         mp.gpci,
                         cf = 32.7442) {
  ((wk.rvu * wk.gpci) + (pe.rvu * pe.gpci) + (mp.rvu * mp.gpci)) * cf
}


#' PFS RVU 2024
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' rvu()
#' @export
rvu <- function() {
  pins::pin_read(mount_board(), "rvu")
}

#' PFS Payment Amount 2024
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' payment()
#' @export
payment <- function() {
  pins::pin_read(mount_board(), "pymt")
}

#' GPCIs 2024
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' gpci()
#' @export
gpci <- function() {
  pins::pin_read(mount_board(), "gpci")
}
