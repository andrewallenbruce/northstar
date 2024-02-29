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
