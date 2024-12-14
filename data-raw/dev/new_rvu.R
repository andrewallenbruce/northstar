new_rvu <- function(wk, pe, mp) {

  stopifnot(
    is.numeric(wk),
    is.numeric(pe),
    is.numeric(mp),
    length(wk) == length(pe),
    length(wk) == length(mp)
  )

  structure(
    list(
      wk = wk,
      pe = pe,
      mp = mp
      ),
    class = "rvu"
  )
}



new_rvu <- function(x = double(), component = "work") {

  if (!rlang::is_double(x)) {
    rlang::abort("`x` must be a double vector.")
  }

  component <- match.arg(component, c("work", "pe_non", "pe_fac", "mp"))

  vctrs::new_vctr(x, class = c("rvu", component = component))

}

x1 <- new_rvu(c(1, 2, 3), component = "work")

x1

rvu <- function(x = double(), component = "work") {
  x <- vctrs::vec_cast(x, double())
  new_rvu(x, component = component)
}


new_rvu()
rvu()

is_rvu <- function(x) {
  inherits(x, "rvu")
}

is_rvu(x1)

format.rvu <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

data.frame(x1)

vec_ptype_abbr.rvu <- function(x, ...) {
  "rvu"
}

dplyr::tibble(x1)

str(x1)

print.rvu <- function(x, ...) {
  cat(
    sprintf(
      "An `<rvu>` object with %s rows and %s cols",
      dim(x)[1], dim(x)[2]
    )
  )
  invisible(x)
}
