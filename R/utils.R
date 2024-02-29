#' Return GitHub raw url
#' @noRd
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

#' Wrapper to mount package's [pins::board_url()]
#' @noRd
mount_board <- function() {
  pins::board_url(gh_raw(
    "andrewallenbruce/northstar/master/pins/"))
}

#' Search a data frame's column by string
#' @param df data frame
#' @param col column to search
#' @param search string to search
#' @param ignore ignore string case?
#' @return collapsed character vector
#' @noRd
srchcol <- function(df, col, search, ignore = FALSE) {
  dplyr::filter(df, stringr::str_detect(
    !!rlang::sym(col), stringr::regex(search, ignore_case = ignore)))
}
