#' National Coverage Determinations
#'
#' Database Last Updated 2022-12-08
#'
#' @param coverage Coverage level; `"Full"`, `"Restricted"`, `"None"`, `"Unknown"`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' ncd(coverage = "Unknown")
#'
#' @export
#'
#' @autoglobal
ncd <- function(coverage = NULL, ...) {

  ncd <- pins::pin_read(mount_board(), "ncd")

  if (!is.null(coverage)) {

    coverage <- rlang::arg_match(
      coverage,
      c("Full", "Restricted", "None", "Unknown")
      )
    ncd <- vctrs::vec_slice(ncd, ncd$coverage == coverage)
  }
  return(ncd)
}

#' Local Coverage Determinations
#'
#' Database Last Updated 2023-04-27
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' lcd(hcpcs = c("A4555", "E0766"))
#'
#' @export
#'
#' @autoglobal
lcd <- function(hcpcs = NULL, ...) {

  lcd <- pins::pin_read(mount_board(), "lcd")

  lcd <- fuimus::search_in_if(lcd, lcd$hcpc_code_id, hcpcs)

  return(lcd)
}
