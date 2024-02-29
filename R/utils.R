#' Return GitHub raw url
#' @param x url
#' @returns raw url
#' @examples
#' github_raw("andrewallenbruce/provider/")
#' @export
github_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}
