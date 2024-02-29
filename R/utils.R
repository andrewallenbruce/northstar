#' Return GitHub raw url
#' @param x url
#' @returns raw url
#' @examples
#' github_raw("andrewallenbruce/provider/")
#' @export
github_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

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
