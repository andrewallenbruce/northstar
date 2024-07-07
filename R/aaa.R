
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
.as <- list(
  chr = \(...) as.character(...),
  int = \(...) as.integer(...),
  dbl = \(...) as.double(...),
  lgl = \(...) as.logical(...),
  fct = \(...) as.factor(...),
  dte = \(...) as.Date(...),
  vct = \(...) as.vector(...)
)
