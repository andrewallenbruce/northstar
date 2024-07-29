#' Construct regex patterns
#'
#' @param x `<chr>` vector
#'
#' @examples
#' construct_regex(search_descriptions()$hcpcs_code)
#'
#' construct_regex(search_hcpcs()$hcpcs_code)
#'
#' # Incorrect:
#' construct_regex(search_adjustments()$adj_code)
#'
#' # Should be:
#' "^[A-DM-PWY1-9][AIOR0-9]?[0-9]{0,3}?$"
#'
#' # Test adj codes
#' c("4", "CO", "P6", "100", "B19", "MA22", "MA124", "N766")
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @export
construct_regex <- function(x) {

  uniq_nona <- \(x) collapse::funique(collapse::na_rm(x))

  x <- gsub(" ", "", uniq_nona(x))

  # stringi::stri_split_boundaries(
  # x,
  # type = "character",
  # simplify = TRUE)

  vecs <- stringr::str_split_fixed(
    x, "",
    n = max(
      collapse::vlengths(x)
      )
    ) |>
    as.data.frame() |>
    purrr::map(dplyr::na_if, y = "")

  to_brackets <- vecs |>
    purrr::map(uniq_nona) |>
    purrr::map(pos_re)

  qmark <- names(which(purrr::map_lgl(vecs, anyNA)))

  if (!vctrs::vec_is_empty(qmark)) {
    to_brackets[qmark] <- purrr::map(
      to_brackets[qmark], \(x) paste0(x, "?"))
  }

  to_vec <- to_brackets |>
    purrr::map(id_runs) |>
    purrr::list_c()

  if (collapse::any_duplicated(to_vec)) {

    # TODO probably need to vectorize this, will surely
    # have more than one unique duplicate out of order

    dupe_idx <- which(collapse::fduplicated(to_vec, all = TRUE))

    rp <- paste0(to_vec[dupe_idx][1], "{", length(dupe_idx), "}")

    to_vec[dupe_idx] <- rp

    to_vec <- collapse::funique(to_vec)

  }

  x <- paste0("^", fuimus::collapser(to_vec), "$")

  return(x)
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

  paste0(
    fuimus::collapser(alphabet),
    fuimus::collapser(numbers)
  )
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
id_runs <- function(x) {

  vec <- c(LETTERS, 0:9)

  vec <- rlang::set_names(rep(0, length(vec)), vec)

  test <- fuimus::splitter(x)

  vecna <- vec[test]

  vecna <- vecna[!is.na(vecna)]

  vec[names(vecna)] <- 1

  vec_group <- dplyr::tibble(
    value = names(vec),
    key = vec,
    idx = 1:length(vec),
    group = dplyr::consecutive_id(key)
    ) |>
    dplyr::mutate(
      group_size = dplyr::n(),
      .by = group
      ) |>
    dplyr::filter(
      key == 1,
      group_size >= 3
      ) |>
    dplyr::select(
      value,
      group
      )

  if (vctrs::vec_is_empty(vec_group)) return(x)

  xgroups <- unname(
    split(
      vec_group,
      vec_group$group
      )
    ) |>
    purrr::map(
      purrr::pluck("value")
      ) |>
    purrr::map(
      paste0,
      collapse = ""
      ) |>
    purrr::list_c()

  replacements <- dplyr::left_join(
    dplyr::slice_min(
      vec_group,
      by = group,
      order_by = value
      ) |>
      dplyr::rename(start = value),
    dplyr::slice_max(
      vec_group,
      by = group,
      order_by = value
      ) |>
      dplyr::rename(end = value),
    by = dplyr::join_by(group)
  ) |>
    glue::glue_data(
      "{start}-{end}"
      ) |>
    as.vector()

  res <- stringi::stri_replace_all_regex(
    x,
    xgroups,
    replacements,
    vectorize_all = FALSE)

  paste0("[", res, "]")
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

#' Construct regex patterns
#'
#' @param x `<chr>` vector
#'
#' @examples
#' construct_regex_old(search_descriptions()$hcpcs_code)
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @noRd
construct_regex_old <- function(x) {

  # TODO: check for equal lengths

  x <- collapse::funique(
    collapse::na_rm(
      gsub(" ", "", x)))

  x <- stringr::str_split(x, "") |>
    purrr::list_transpose() |>
    purrr::map(collapse::funique) |>
    purrr::map(pos_re_old) |>
    purrr::list_c() |>
    paste0(collapse = "")

  paste0("^", x, "$")

}

#' Internal function for `construct_regex_old()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_re_old <- function(x) {

  sorted   <- stringr::str_sort(x, numeric = TRUE)
  alphabet <- purrr::list_c(strex::str_extract_non_numerics(sorted))
  numbers  <- purrr::list_c(strex::str_extract_numbers(sorted))

  paste0("[",
         fuimus::collapser(alphabet),
         fuimus::collapser(numbers),
         "]")

}
