#' 2024 National Physician Fee Schedule Relative Value File
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' rvu()
#' @export
rvu <- function() {
  pins::pin_read(mount_board(), "rvu")
}

#' 2024 Physician Fee Schedule Payment Amount File
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' payment()
#' @export
payment <- function() {
  pins::pin_read(mount_board(), "pymt")
}

#' 2024 Geographic Practice Cost Indices
#' @param search description
#' @param col description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' gpci()
#' @export
gpci <- function(search = NULL,
                 col = c("mac",
                         "state",
                         "locality",
                         "locality_name",
                         "code")) {

  gpci <- pins::pin_read(mount_board(), "gpci")
  col <- match.arg(col)

  if (!is.null(search)) gpci <- srchcol(gpci, col = col, search = search, ignore = TRUE)
  return(gpci)
}

#' 2024 Healthcare Common Procedure Coding System (HCPCS)
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' hcpcs()
#' @export
hcpcs <- function() {
  pins::pin_read(mount_board(), "hcpcs")
}
