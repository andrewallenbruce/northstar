#' Construct regex patterns
#'
#' @param x `<chr>` vector
#'
#' @examples
#' construct_regex(search_descriptions()$hcpcs_code)
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
# @keywords internal
#'
#' @export
construct_regex <- function(x) {

  # TODO: check for equal lengths

  x <- collapse::funique(
    collapse::na_rm(
      gsub(" ", "", x)))

  x <- stringr::str_split(x, "") |>
    purrr::list_transpose() |>
    purrr::map(collapse::funique) |>
    purrr::map(pos_re) |>
    purrr::list_c() |>
    paste0(collapse = "")

  paste0("^", x, "$")

}

#' Internal function for `construct_regex()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_re <- function(x) {

  sorted   <- stringr::str_sort(x, numeric = TRUE)
  alphabet <- purrr::list_c(strex::str_extract_non_numerics(sorted))
  numbers  <- purrr::list_c(strex::str_extract_numbers(sorted))

  paste0("[",
         fuimus::collapser(alphabet),
         fuimus::collapser(numbers),
         "]")

}

#' Construct regex patterns
#'
#' @param x `<chr>` vector
#'
#' @examples
#' construct_regex2(search_descriptions()$hcpcs_code)
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
# @keywords internal
#'
#' @export
construct_regex2 <- function(x) {

  x <- collapse::funique(
    collapse::na_rm(
      gsub(" ", "", x)
    )
  )

  vecs <- stringr::str_split_fixed(
    x,
    "",
    n = max(
      collapse::vlengths(x)
    )
  ) |>
    as.data.frame() |>
    purrr::map(
      dplyr::na_if,
      y = ""
      )

  to_brackets <- vecs |>
    purrr::map(collapse::na_rm) |>
    purrr::map(collapse::funique) |>
    purrr::map(pos_re2)

  qmark <- names(
    which(
      purrr::map_lgl(vecs, anyNA)
      )
    )

  if (!vctrs::vec_is_empty(qmark)) {
    to_brackets[qmark] <- purrr::map(
      to_brackets[qmark],
      \(x) paste0(x, "?")
      )
  }

  to_vec <- to_brackets |>
    purrr::list_c() |>
    paste0(collapse = "")

  paste0("^", to_vec, "$")
}

#' Internal function for `construct_regex2()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_re2 <- function(x) {

  sorted   <- stringr::str_sort(x, numeric = TRUE)
  alphabet <- purrr::list_c(strex::str_extract_non_numerics(sorted))
  numbers  <- purrr::list_c(strex::str_extract_numbers(sorted))

  paste0("[",
         fuimus::collapser(alphabet),
         fuimus::collapser(numbers),
         "]",
         "{1}"
         )

}

#' Internal function for `construct_regex2()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_nchar <- function(x) {

  ch <- range(collapse::vlengths(x))

  ifelse(
    ch[1] == ch[2],
    paste0("{", ch[1], "}"),
    paste0("{", ch[1], ",", ch[2], "}")
  )

}
