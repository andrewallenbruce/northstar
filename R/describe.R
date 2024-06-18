#' Describe HCPCS Codes
#'
#' @template args-hcpcs
#'
#' @param desc_type `<chr>` vector of code description types; `All` (default),
#'   `Short`, `Long`, `Medium`, `Medical`, `Consumer`, `Clinician`, `Proprietary Name`
#'
#' @param varname If `hcpcs` is a [data.frame] or a
#'   [tibble][tibble::tibble-package], this is the quoted name of the column
#'   containing HCPCS codes; default is `"hcpcs"`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' describe_hcpcs(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' describe_hcpcs(
#'   dplyr::tibble(
#'     hcpcs = c("A0021", "V5362", "J9264", "G8916")))
#'
#' @autoglobal
#'
#' @export
describe_hcpcs <- function(hcpcs = NULL, desc_type = "All", varname = "hcpcs", ...) {

  dsc <- get_pin("hcpcs_descriptions")

  desc_type <- match.arg(
    desc_type,
    c("All", "Short", "Long",
      "Medium", "Medical", "Consumer",
      "Clinician", "Proprietary Name"),
    several.ok = TRUE
  )

  if (!is.null(hcpcs)) {

    obj_type <- names(
      which(c(vec = is.vector(hcpcs),
              dfr = is.data.frame(hcpcs))))

    dsc <- switch(
      obj_type,
      vec = fuimus::search_in(dsc, dsc$hcpcs, hcpcs),
      dfr = fuimus::search_in(dsc, dsc$hcpcs, hcpcs[[varname]]))
  }

  if (desc_type != "All") {

    dsc <- fuimus::search_in(dsc, dsc$desc_type, desc_type)
  }
  return(.add_class(dsc))
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
