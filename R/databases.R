#' 2024 National Physician Fee Schedule Relative Value File
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' rvu(hcpcs = "11646")
#' @export
rvu <- function(hcpcs = NULL) {

  rv <- pins::pin_read(mount_board(), "rvu")

  if (!is.null(hcpcs)) {rv <- vctrs::vec_slice(rv, rv$hcpcs == hcpcs)}

  return(rv)
}

#' 2024 Physician Fee Schedule Payment Amount File
#' @param hcpcs description
#' @param mac description
#' @param locality description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' pfs(hcpcs    = c("39503", "43116", "33935", "11646"),
#'     locality = "01",
#'     mac      = "10212")
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

  gp <- pins::pin_read(mount_board(), "gpci")
  # gp$state <- as.character(gp$state)

  if (!is.null(mac))      {gp <- vctrs::vec_slice(gp, gp$mac      == mac)}
  if (!is.null(state))    {gp <- vctrs::vec_slice(gp, gp$state    == state)}
  if (!is.null(locality)) {gp <- vctrs::vec_slice(gp, gp$locality == locality)}

  return(gp)
}

#' 2024 Healthcare Common Procedure Coding System (HCPCS)
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' hcpcs_lv2(hcpcs = c("39503", "43116", "33935", "11646"))
#' @export
hcpcs_lv2 <- function(hcpcs = NULL) {

  l2 <- pins::pin_read(mount_board(), "hcpcs")

  # if (!is.null(hcpcs)) L2 <- dplyr::filter(L2, hcpcs %chin% hcpcs)
  if (!is.null(hcpcs)) l2 <- vctrs::vec_slice(l2, l2$hcpcs == hcpcs)

  return(l2)

}

#' 2023 CPT Descriptors (Clinician & Consumer-Friendly)
#' @param hcpcs description
#' @return a [dplyr::tibble()]
#' @examplesIf interactive()
#' cpt_descriptors()
#' @export
cpt_descriptors <- function(hcpcs = NULL) {

  cpt <- pins::pin_read(mount_board(), "cpt_descriptors")

  # if (!is.null(hcpcs)) cpt <- dplyr::filter(cpt, hcpcs %chin% hcpcs)
  if (!is.null(hcpcs)) cpt <- vctrs::vec_slice(cpt, cpt$cpt == hcpcs)

  return(cpt)

}
