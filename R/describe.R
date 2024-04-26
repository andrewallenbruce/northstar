# search_hcpcs(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
# search_hcpcs <- function(hcpcs   = NULL,
#                          columns = c("limit", "full"),
#                          ...) {
#
#   limited <- vctrs::vec_c(
#     "hcpcs",
#     "description_short",
#     "description_long",
#     "price",
#     "multi_price",
#     "labcert",
#     "xref",
#     "tos",
#     "coverage",
#     "asc",
#     "betos"
#   )
#
#   columns <- match.arg(columns)
#
#   lv2 <- switch(
#     columns,
#     limit  = get_pin("hcpcs")[limited],
#     full   = get_pin("hcpcs")
#   )
#
#   lv2 <- fuimus::search_in_if(lv2, lv2$hcpcs, hcpcs)
#
#   return(lv2)
# }

#' HCPCS Level I (CPT) Codes
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_cpt(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' @export
#'
#' @autoglobal
search_cpt <- function(hcpcs = NULL, ...) {

  cpt <- get_pin("cpt_descriptors")
  cpt <- fuimus::search_in_if(cpt, cpt$hcpcs, hcpcs)
  return(cpt)
}

#' HCPCS Level II Codes
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_level_two(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
#'
#' @export
#'
#' @autoglobal
search_level_two <- function(hcpcs = NULL, ...) {

  two <- get_pin("two_descriptions")
  two <- fuimus::search_in_if(two, two$hcpcs, hcpcs)
  return(two)
}

#' HCPCS code type
#'
#' @template args-hcpcs
#'
#' @template args-dots
#'
#' @returns A `<list>` of three `<chr>` vectors indicating set membership:
#'   `cpt`, `hcpcs`, and `neither`
#'
#' @examples
#' # Only Category I CPTs
#' get_hcpcs_type(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' # Only Level II HCPCS
#' get_hcpcs_type(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
#'
#' # Both
#' get_hcpcs_type(hcpcs = c("39503", "43116", "A0021", "V5362"))
#'
#' # Neither
#' get_hcpcs_type(hcpcs = c("CCCCC", "0002U", "0003U", "0004U", "1164F", "0074T"))
#'
#' # All Three
#' get_hcpcs_type(hcpcs = c("39503", "43116", "A0021", "V5362", "1164F", "0074T"))
#'
#' @autoglobal
#'
#' @export
get_hcpcs_type <- function(hcpcs, ...) {

  cvec <- get_pin("cpt_hcpcs_vecs")

  hcpcs <- collapse::funique(hcpcs)

  list(
    cpt     = fuimus::search_in(hcpcs, hcpcs, cvec$cpt),
    hcpcs   = fuimus::search_in(hcpcs, hcpcs, cvec$hcpcs),
    neither = vctrs::vec_set_difference(hcpcs,
              vctrs::vec_c(cvec$cpt, cvec$hcpcs))
  )
}


#' @autoglobal
#'
#' @noRd
describe_hcpcs <- function(hcpcs, ...) {

  rlang::check_required(hcpcs)

  x <- list(
    ds = search_cpt(hcpcs = hcpcs),
    l2 = search_level_two(hcpcs = hcpcs),
    rb = search_rbcs(hcpcs = hcpcs)
  )

  x <- list(
    cpt = null_if_empty(x$ds),
    lvl = null_if_empty(x$l2),
    rbc = null_if_empty(x$rb)
    ) |>
    purrr::compact()

    # sort(
    #   lengths(
    #   list(
    #     cpt = collapse::funique(x$cpt$hcpcs),
    #     lvl = collapse::funique(x$lvl$hcpcs),
    #     rbc = collapse::funique(x$rbc$hcpcs)
    #     )
    #   ), decreasing = TRUE
    # )

  vctrs::vec_rbind(x$cpt, x$lvl)

  # create `join_by` object
  by_hcpcs <- dplyr::join_by(hcpcs)

  # test if x contains only cpt codes
  lvl1 <- rlang::has_name(x, "cpt") & !rlang::has_name(x, "lvl")

  # test if x contains only hcpcs codes
  lvl2 <- rlang::has_name(x, "lvl") & !rlang::has_name(x, "cpt")

  # test if x contains both hcpcs and cpt codes
  both <- all(rlang::has_name(x, c("lvl", "cpt")))

  # only one should be true, extract its name
  path <- list(
    one  = if (lvl1) lvl1 else NULL,
    two  = if (lvl2) lvl2 else NULL,
    both = if (both) both else NULL
    ) |>
    purrr::compact() |>
    names()

  # perform join based on path
  res <- switch(
    path,
    one = dplyr::left_join(res, x$pay, bypctc) |> dplyr::left_join(x$cpt, byhcpc),
    both = dplyr::left_join(res, x$lvl, byhcpc) |> dplyr::left_join(x$pay, bypctc) |> dplyr::left_join(x$cpt, byhcpc),
    two = dplyr::left_join(res, x$lvl, byhcpc)
  )

  dplyr::left_join(x$rbc, x$cpt, by = "hcpcs")
}
