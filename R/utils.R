#' Return GitHub raw url
#' @noRd
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

#' Mount [pins][pins::pins-package] board
#' @param source `"local"` or `"remote"`
#' @return `<pins_board_folder>` or `<pins_board_url>`
#' @noRd
mount_board <- function(source = c("local", "remote")) {

  source <- match.arg(source)

  switch(
    source,
    local  = pins::board_folder(fs::path_package("extdata/pins", package = "northstar")),
    remote = pins::board_url(gh_raw(
      "andrewallenbruce/northstar/master/inst/extdata/pins/"
    ))
  )
}

#' Search a data frame column by string
#' @param df data frame
#' @param col bare column name
#' @param search string
#' @param ignore ignore case, default is `TRUE`
#' @param ... Empty
#' @return a [tibble][tibble::tibble-package]
#' @noRd
srchcol <- function(df,
                    col,
                    search,
                    ignore = TRUE,
                    ...) {

  dplyr::filter(df, stringr::str_detect(
    !!rlang::sym(col),
    stringr::regex(search, ignore_case = ignore)
  ))
}

#' Infix operator for `if (!is.null(x)) y else x` statements
#' @param x,y vectors
#' @return `y` if `x` is not `NULL`, else `x`
#' @examples
#' ccn <- NULL
#' ccn %nn% 123456L
#' @autoglobal
#' @noRd
`%nn%` <- function(x, y) {
  if (!is.null(x))
    y
  else
    x
}

#' Infix operator for `not in` statements
#' @return description
#' @autoglobal
#' @noRd
`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' Convert empty char values to NA
#' @param x vector
#' @examples
#' na_blank(x = c(" ", "*", "--", "N/A", ""))
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
  stopifnot("Input must be a named vector" = is.null(names(x)))
  rlang::set_names(names(x), unname(x))
}

#' Pivot data frame to long format for easy printing
#' @param df data frame
#' @param cols vector of columns to pivot long, default is [dplyr::everything()]
#' @autoglobal
#' @export
#' @keywords internal
display_long <- function(df, cols = dplyr::everything()) {

  df |> dplyr::mutate(
    dplyr::across(
      dplyr::everything(), as.character)) |>
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
