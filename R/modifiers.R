#' Level I and II HCPCS Modifiers
#'
#' A modifier provides the means to report or indicate that a service or
#' procedure that has been performed has been altered by some specific
#' circumstance but not changed in its definition or code.
#'
#' Modifiers also enable health care professionals to effectively respond to
#' payment policy requirements established by other entities.
#'
#' @param modifier `<chr>` vector of 2-character HCPCS modifiers; default is
#'   `NULL`
#'
#' @param modifier_type `<chr>` Modifier type, one of `HCPCS`, `CPT`,
#'   `Anesthesia`, or `Performance Measure`; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_modifiers(modifier = c("25", "59"))
#'
#' search_modifiers(modifier_type = "CPT")
#'
#' @export
#'
#' @autoglobal
search_modifiers <- function(modifier = NULL, modifier_type = NULL, ...) {

  md <- get_pin("modifiers")

  md <- fuimus::search_in_if(md, md$modifier_type, modifier_type)
  md <- fuimus::search_in_if(md, md$modifier, modifier)

  return(.add_class(md))

}
