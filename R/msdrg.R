#' Medicare Severity Diagnosis-Related Groups (MS-DRG)
#'
#' The Medicare Severity Diagnosis-Related Group (MS-DRG) is a classification
#' system used by the Centers for Medicare and Medicaid Services (CMS) to group
#' patients with similar clinical characteristics and resource utilization into
#' a single payment category.
#'
#' The system is primarily used for Medicare reimbursement purposes, but it is
#' also adopted by many other payers as a basis for payment determination.
#'
#' MS-DRGs are based on the principal diagnosis, up to 24 additional diagnoses,
#' and up to 25 procedures performed during the stay. In a small number of
#' MS-DRGs, classification is also based on the age, sex, and discharge status
#' of the patient.
#'
#' Hospitals serving more severely ill patients receive increased
#' reimbursements, while hospitals treating less severely ill patients will
#' receive less reimbursement.
#'
#' @param drg *<chr>* vector of 3-digit Diagnosis-Related Group (DRG) code
#' @param mdc *<chr>* vector of 2-digit Major Diagnostic Category (MDC) code
#' @param type *<chr>* DRG type: `Medical` or `Surgical`
#' @param ... Empty
#' @return A [tibble][tibble::tibble-package]
#' @examples
#' msdrg(drg = "011")
#'
#' msdrg(mdc = "24")
#'
#' msdrg(type = "Medical")
#' @autoglobal
#' @export
msdrg <- function(drg  = NULL,
                  mdc  = NULL,
                  type = NULL,
                  ...) {

  ms <- pins::pin_read(mount_board(), "msdrg")

  if (!is.null(type)) {
    ms <- vctrs::vec_slice(ms, ms$drg_type == type)
  }

  if (!is.null(drg)) {
    ms <- vctrs::vec_slice(ms,
                           vctrs::vec_in(ms$drg,
                                         collapse::funique(drg)))
  }

  if (!is.null(mdc)) {
    ms <- vctrs::vec_slice(ms,
                           vctrs::vec_in(ms$mdc,
                                         collapse::funique(mdc)))
  }
  return(ms)
}
