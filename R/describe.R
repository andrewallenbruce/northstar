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
describe_hcpcs <- function(hcpcs_code = NULL, desc_type = "All", varname = "hcpcs", ...) {

  dsc <- get_pin("hcpcs_descriptions")

  desc_type <- match.arg(
    desc_type,
    c("All", "Short", "Long",
      "Medium", "Medical", "Consumer",
      "Clinician", "Proprietary Name"),
    several.ok = TRUE
  )

  if (!is.null(hcpcs_code)) {

    obj_type <- names(
      which(c(vec = is.vector(hcpcs_code),
              dfr = is.data.frame(hcpcs_code))))

    dsc <- switch(
      obj_type,
      vec = fuimus::search_in(dsc, dsc$hcpcs, hcpcs_code),
      dfr = fuimus::search_in(dsc, dsc$hcpcs, hcpcs_code[[varname]]))
  }

  if (desc_type != "All") {

    dsc <- fuimus::search_in(dsc, dsc$desc_type, desc_type)
  }
  return(.add_class(dsc))
}
