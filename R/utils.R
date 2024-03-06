#' Return GitHub raw url
#' @noRd
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

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

#' @param abb state abbreviation
#' @return state full name
#' @autoglobal
#' @noRd
abb2full <- function(abb,
                     arg = rlang::caller_arg(abb),
                     call = rlang::caller_env()) {

  results <- dplyr::tibble(x = c(state.abb[1:8],
                                 'DC',
                                 state.abb[9:50],
                                 'AS', 'GU', 'MP', 'PR', 'VI', 'UK'),
                           y = c(state.name[1:8],
                                 'District of Columbia',
                                 state.name[9:50],
                                 'American Samoa',
                                 'Guam',
                                 'Northern Mariana Islands',
                                 'Puerto Rico',
                                 'Virgin Islands',
                                 'Unknown')) |>
    dplyr::filter(x == abb) |>
    dplyr::pull(y)

  if (vctrs::vec_is_empty(results)) {
    cli::cli_abort(c("{.val {abb}} is not a valid state abbreviation."), # nolint
                   call = call)
  }
  return(results)
}
