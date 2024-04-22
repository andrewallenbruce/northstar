#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @noRd
mount_board <- function(source = c("local", "remote")) {

  source <- match.arg(source)

  switch(
    source,
    local  = pins::board_folder(
             fs::path_package(
               "extdata/pins",
               package = "northstar")
             ),
    remote = pins::board_url(
      fuimus::gh_raw(
      "andrewallenbruce/northstar/master/inst/extdata/pins/")
      )
    )
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @template args-dots
#'
#' @returns `<tibble>`
#'
#' @noRd
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match(pin, pins::pin_list(board = board))

  pins::pin_read(board, pin)

}

#' Search in data frame column if search term is not `NULL`
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param dfcol A `<character>` or `<symbol>` specifying the column to search in
#'
#' @param search A `<character>` or `<symbol>` specifying the search term
#'
#' @param args A `<character>` vector of argument options; default is `NULL`
#'
#' @param multiple A `<logical>` indicating if multiple `search` args are
#'   allowed; default is `FALSE`
#'
#' @returns A `<data.frame>` or `<tibble>`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10, z = letters[1:10])
#'
#' search_in_if_args(df = x, dfcol = x$z, search = c("a", "j"))
#'
#' search_in_if_args(df = x, dfcol = x$z, search = NULL)
#'
#' search_in_if_args(df = x,
#'                   dfcol = x$z,
#'                   search = c("a", "j"),
#'                   args = c("a", "j"),
#'                   multiple = TRUE)
#'
#' try(search_in_if_args(df = x,
#'                       dfcol = x$z,
#'                       search = c("a", "j"),
#'                       args = c("a", "z"),
#'                       multiple = FALSE))
#'
#' @autoglobal
#'
#' @export
search_in_if_args <- function(df,
                              dfcol,
                              search,
                              args = NULL,
                              multiple = FALSE) {

  if (!is.null(search)) {

    if (!is.null(args)) {
      search <- rlang::arg_match(
        arg = search,
        values = args,
        multiple = multiple)
    }

    vctrs::vec_slice(df,
    vctrs::vec_in(dfcol,
    collapse::funique(search)))

  } else {
    df
    }
}

#' @autoglobal
#'
#' @noRd
null_if_empty <- function(x) {
  if (vctrs::vec_is_empty(x)) NULL else x
}

#' @autoglobal
#'
#' @noRd
add_ifelse <- function(x,
                       df,
                       dfcol,
                       by) {

  if (vctrs::vec_is_empty(x)) {
    NULL
    } else {
      vctrs::vec_slice(df,
      vctrs::vec_in(dfcol, x)) |>
      tidyr::nest(.by = {{ by }}) }
  }
