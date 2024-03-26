#' Return GitHub raw url
#' @noRd
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

# compact <- function(.x) Filter(length, .x)

#' Wrapper to mount package's [pins::board_url()]
#' @noRd
mount_board <- function() {
  pins::board_url(gh_raw("andrewallenbruce/northstar/master/pins/"))
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

#' Infix operator for `if (!is.null(x)) y else x` statements
#' @param x,y description
#' @return description
#' @examples
#' ccn <- 123456
#' ccn <- ccn %nn% as.character(ccn)
#' ccn
#' @autoglobal
#' @noRd
`%nn%` <- function(x, y) if (!is.null(x)) y else x #nocov

#' Infix operator for `not in` statements
#' @return description
#' @autoglobal
#' @noRd
`%nin%` <- function(x, table) match(x, table, nomatch = 0L) == 0L #nocov

#' Convert empty char values to NA
#' @param x vector
#' @autoglobal
#' @noRd
na_blank <- function(x) {

  x <- dplyr::na_if(x, "")
  x <- dplyr::na_if(x, " ")
  x <- dplyr::na_if(x, "*")
  x <- dplyr::na_if(x, "--")
  x <- dplyr::na_if(x, "N/A")
  return(x)
}

#' Invert a named vector
#' @param x A named vector
#' @return A named vector with names and values inverted
#' @examples
#' invert_named(x = c("name" = "element"))
#' invert_named(x = c("element" = "name"))
#' @autoglobal
#' @noRd
invert_named <- function(x) {
  if(is.null(names(x))) {
    stop("Input must be a named vector.")
  }
  rlang::set_names(names(x), unname(x))
}

#' Pivot data frame to long format for easy printing
#' @param df data frame
#' @param cols vector of columns to pivot long, default is [dplyr::everything()]
#' @autoglobal
#' @export
#' @keywords internal
display_long <- function(df, cols = dplyr::everything()) {

  df |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer({{ cols }})
}

#' Format multiple line character vector to single line
#' @param x character vector
#' @autoglobal
#' @export
#' @keywords internal
single_line_string <- function(x) {
  stringr::str_remove_all(x, r"(\n\s*)")
}
