#' Level I and II HCPCS Modifiers
#'
#' A modifier provides the means to report or indicate that a service or
#' procedure that has been performed has been altered by some specific
#' circumstance but not changed in its definition or code.
#'
#' Modifiers also enable health care professionals to effectively respond to
#' payment policy requirements established by other entities.
#'
#' @param mod `<chr>` vector of 2-character HCPCS modifiers; default is `NULL`
#'
#' @template args-dots
#'
#' @template returns
#'
#' @examples
#' search_modifiers(mod = c("25", "59"))
#'
#' @export
#'
#' @autoglobal
search_modifiers <- function(mod = NULL, ...) {

  md <- pins::pin_read(mount_board(), "modifiers")

  if (!is.null(mod)) {
    md <- search_in(md, md$mod, mod)
  }
  return(md)
}
