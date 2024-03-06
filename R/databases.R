#' 2024 National Physician Fee Schedule Relative Value File
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' rvu(hcpcs = "11646")
#' @export
rvu <- function(hcpcs = NULL) {

  rvu <- pins::pin_read(mount_board(), "rvu")

  if (!is.null(hcpcs)) {rvu <- vctrs::vec_slice(rvu, rvu$hcpcs == hcpcs)}

  return(rvu)
}

#' 2024 Physician Fee Schedule Payment Amount File
#' @param hcpcs description
#' @param mac description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' pfs(hcpcs = "11646", locality = "01", mac = "10212")
#' @export
pfs <- function(hcpcs    = NULL,
                mac      = NULL,
                locality = NULL) {

  pmt <- pins::pin_read(mount_board(), "pymt")

  if (!is.null(hcpcs))    {pmt <- vctrs::vec_slice(pmt, pmt$hcpcs    == hcpcs)}
  if (!is.null(mac))      {pmt <- vctrs::vec_slice(pmt, pmt$mac      == mac)}
  if (!is.null(locality)) {pmt <- vctrs::vec_slice(pmt, pmt$locality == locality)}

  return(pmt)

}

#' 2024 Geographic Practice Cost Indices
#' @param mac description
#' @param state description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' gpci(state = "GA", locality = "01", mac = "10212")
#' @export
gpci <- function(mac      = NULL,
                 state    = NULL,
                 locality = NULL) {

  gpci <- pins::pin_read(mount_board(), "gpci")

  if (!is.null(mac))      {gpci <- vctrs::vec_slice(gpci, gpci$mac      == mac)}
  if (!is.null(state))    {gpci <- vctrs::vec_slice(gpci, gpci$state    == state)}
  if (!is.null(locality)) {gpci <- vctrs::vec_slice(gpci, gpci$locality == locality)}

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

#' 2023 CPT Descriptors (Clinician & Consumer-Friendly)
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' cpt_descriptors()
#' @export
cpt_descriptors <- function() {
  pins::pin_read(mount_board(), "cpt_descriptors")
}
