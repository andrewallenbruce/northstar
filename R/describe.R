#' Get HCPCS Descriptions
#'
#' @template args-hcpcs
#'
#' @param desc_type `<chr>` vector of description types; `All` (default),
#'   `Short`, `Long`, `Medium`, `Medical`, `Consumer`, `Clinician`,
#'   `Proprietary Name`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' get_descriptions(hcpcs = c("39503", "43116", "33935", "11646"))
#'
#' get_descriptions(hcpcs = c("A0021", "V5362", "J9264", "G8916"))
#'
#' @export
#'
#' @autoglobal
get_descriptions <- function(hcpcs = NULL, desc_type = "All", ...) {

  dsc <- get_pin("hcpcs_descriptions")
  dsc <- fuimus::search_in_if(dsc, dsc$hcpcs, hcpcs)

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
