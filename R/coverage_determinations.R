#' National Coverage Determinations
#'
#' @note Database Last Updated 2022-12-08
#'
#' @param coverage `<chr>` vector of coverage levels; `"Full"`, `"Restricted"`,
#'   `"None"`, `"Unknown"`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' ncd(coverage = c("Unknown", "None"))
#'
#' try(ncd(coverage = c("Unknown", "Noe")))
#'
#' @export
#'
#' @autoglobal
ncd <- function(coverage = NULL, ...) {

  ncd <- get_pin("ncd")

  ncd <- search_in_if_args(
    ncd,
    ncd$coverage,
    coverage,
    args = c("Full", "Restricted", "None", "Unknown"),
    multiple = TRUE)

  return(ncd)
}

#' Local Coverage Determinations
#'
#' @note Database Last Updated 2023-04-27
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

  lcd <- get_pin("lcd")
  lcd <- fuimus::search_in_if(lcd, lcd$hcpc_code_id, hcpcs)
  return(lcd)
}
