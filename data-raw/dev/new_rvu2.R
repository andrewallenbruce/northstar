hcpcs_code = c("95907", "78140", "32820", "61575")

rvu <- search_rvus(hcpcs_code = hcpcs_code)


mac      = "10212"
state    = "GA"
locality = "01"

search_gpcis(mac = mac, state = state, locality = locality)

pos_code = "11"

search_pos(pos_code = pos_code)

search_fee_schedule <- function(hcpcs_code,
                                pos_code = NULL,
                                state    = NULL,
                                locality = NULL,
                                mac      = NULL,
                                ...)

rlang::check_required(hcpcs)

args <- rlang::list2(
  hcpcs_code = hcpcs_code,
  pos_code = pos_code,
  state = state,
  locality = locality,
  mac = mac
)
linestretch: 1.75
# retrieve Relative Value Units
rv <- rlang::inject(search_rvus(!!!args))

# if code not found in rvu, no results in others
msg <- "HCPCS code {.strong {.val {hcpcs}}} not found."
if (vctrs::vec_is_empty(rv)) {cli::cli_abort(msg)}

x <- list(
  rv = rv,
  gp = rlang::inject(search_gpci(!!!args)),
  fs = rlang::inject(search_payment(!!!args)),
  op = rlang::inject(search_opps(!!!args)),
  ds = rlang::inject(search_cpt(!!!args)),
  l2 = rlang::inject(search_hcpcs(!!!args)),
  rb = rlang::inject(search_rbcs(!!!args))
)

x <- list(
  rvu = null_if_empty(x$rv),
  gpc = null_if_empty(x$gp),
  pay = null_if_empty(x$fs),
  opp = null_if_empty(x$op),
  cpt = null_if_empty(x$ds),
  lvl = null_if_empty(x$l2),
  rbc = null_if_empty(x$rb)) |>
  purrr::compact()

# create `join_by` objects
byhcpc <- dplyr::join_by(hcpcs)
bypctc <- dplyr::join_by(hcpcs, mod, status, pctc, mac, locality)
nopctc <- dplyr::join_by(hcpcs, mod, status, mac, locality)

# `cross_join` rvu and gpci, `left_join` rbcs
res <- dplyr::cross_join(x$rvu, x$gpc) |>
  dplyr::left_join(x$rbc, byhcpc)

# test if results contain hcpcs and cpts
both <- all(rlang::has_name(x, c("lvl", "cpt")))

# test if results contain hcpcs only
lvl2 <- rlang::has_name(x, "lvl") & !rlang::has_name(x, "cpt")

# test if results contain cpts only
lvl1 <- rlang::has_name(x, "cpt") & !rlang::has_name(x, "lvl")

# only one should be true, extract its name
path <- list(
  both = if (both) both else NULL,
  lvl2 = if (lvl2) lvl2 else NULL,
  lvl1 = if (lvl1) lvl1 else NULL) |>
  purrr::compact() |>
  names()

# perform join based on path
res <- switch(
  path,
  "both" = dplyr::left_join(res, x$lvl, byhcpc) |>
    dplyr::left_join(x$pay, bypctc) |>
    dplyr::left_join(x$cpt, byhcpc),
  "lvl2" = dplyr::left_join(res, x$lvl, byhcpc),
  "lvl1" = dplyr::left_join(res, x$pay, bypctc) |>
    dplyr::left_join(x$cpt, byhcpc))

# add OPPS data if available
if (rlang::has_name(x, "opp")) {
  res <- dplyr::left_join(res, x$opp, nopctc)
}

res |>
  dplyr::mutate(
    frvus  = janitor::round_half_up(sum(wrvu * wgpci, fprvu * pgpci, mrvu * mgpci), 2),
    nrvus  = janitor::round_half_up(sum(wrvu * wgpci, nfprvu * pgpci, mrvu * mgpci), 2),
    fpar   = janitor::round_half_up(frvus * 32.7442, 2),
    npar   = janitor::round_half_up(nrvus * 32.7442, 2),
    fnpar  = janitor::round_half_up(fpar * 0.95, 2),
    nfnpar = janitor::round_half_up(npar * 0.95, 2),
    flim   = janitor::round_half_up(fpar * 1.0925, 2),
    nlim   = janitor::round_half_up(npar * 1.0925, 2)) |>
  cols_amounts()
