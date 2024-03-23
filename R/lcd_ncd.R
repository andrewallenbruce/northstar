#' National Coverage Determinations
#'
#' Database Last Updated 2022-12-08
#'
#' @param coverage Coverage level; `"Full"`, `"Restricted"`, `"None"`, `"Unknown"`
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' ncd(coverage = "Unknown")
#' @export
#' @autoglobal
ncd <- function(coverage = NULL) {

  ncd <- pins::pin_read(mount_board(), "ncd")

  if (!is.null(coverage)) {

    coverage <- rlang::arg_match(coverage, c("Full", "Restricted", "None", "Unknown"))

    ncd <- vctrs::vec_slice(ncd, ncd$coverage == coverage)
  }
  return(ncd)
}

#' Local Coverage Determinations
#'
#' Database Last Updated 2023-04-27
#'
#' @param hcpcs vector of HCPCS codes
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' lcd(hcpcs = c("A4555", "E0766"))
#' @export
#' @autoglobal
lcd <- function(hcpcs = NULL,
                ...) {

  lcd <- pins::pin_read(mount_board(), "lcd")

  if (!is.null(hcpcs)) {
    lcd <- vctrs::vec_slice(lcd,
           vctrs::vec_in(lcd$hcpc_code_id,
           collapse::funique(hcpcs)))
  }
  return(lcd)
}
