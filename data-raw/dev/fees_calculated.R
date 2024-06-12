# A tibble: 114 × 6
# lobstr::obj_size(gp) == 15.63 kB
gp <- get_pin("gpci") |>
  dplyr::select(-area, -counties)

# A tibble: 9,220 × 5
# lobstr::obj_size(rv) == 793.94 kB
rv <- get_pin("hcpcs_with_rvus") |>
  dplyr::select(-tot_non, -tot_fac, -conv_fct)

# A tibble: 1,051,080 × 11
# lobstr::obj_size(fees) == 92.93 MB
fees <- dplyr::cross_join(rv, gp)

# A tibble: 1,051,080 × 19
# lobstr::obj_size(fees_calculated) == 160.20 MB
fees_calculated <- fees |>
  dplyr::rowwise() |>
  dplyr::mutate(
    rvu_total_fac        = sum(rvu_work * gpci_work, rvu_pe_fac * gpci_pe, rvu_mp * gpci_mp),
    rvu_total_non        = sum(rvu_work * gpci_work, rvu_pe_non * gpci_pe, rvu_mp * gpci_mp),
    participating_fac    = rvu_total_fac * 32.7442,
    participating_non    = rvu_total_non * 32.7442,
    nonparticipating_fac = participating_fac * 0.95,
    nonparticipating_non = participating_non * 0.95,
    limiting_fac         = participating_fac * 1.0925,
    limiting_non         = participating_non * 1.0925) |>
  dplyr::ungroup() |>
  dplyr::select(
    hcpcs,
    mac,
    state,
    locality,
    rvu_work,
    rvu_pe_non,
    rvu_pe_fac,
    rvu_mp,
    gpci_work,
    gpci_pe,
    gpci_mp,
    rvu_total_fac,
    rvu_total_non,
    participating_fac,
    participating_non,
    nonparticipating_fac,
    nonparticipating_non,
    limiting_fac,
    limiting_non
  )

fees_calculated



state = "GA"
locality = "01"
mac = "10212"

hcpcs = c("39503", "43116", "33935", "11646")

hcpcs = c("A0021", "V5362", "J9264", "G8916")

hcpcs = c("39503", "43116", "A0021", "V5362")

hcpcs = c("CCCCC", "0002U", "0003U", "0004U", "1164F", "0074T")

hcpcs = c("39503", "43116", "A0021", "V5362", "1164F", "0074T")

calculate_fees <- function(hcpcs,
                           state    = NULL,
                           locality = NULL,
                           mac      = NULL,
                           ...) {

  args <- rlang::list2(
    hcpcs    = hcpcs,
    state    = state,
    locality = locality,
    mac      = mac
  )

  x <- list(
    rv = rlang::inject(get_rvus(!!!args)),
    gp = rlang::inject(get_gpcis(!!!args))
  )

  x <- list(
    rvu = fuimus::null_if_empty(x$rv),
    gpc = fuimus::null_if_empty(x$gp)
  ) |>
    purrr::compact()

  x <- dplyr::cross_join(x$rvu, x$gpc)

  x |>
    dplyr::rowwise() |>
    dplyr::mutate(
      tot_rvu_fac = sum(rvu_work * gpci_work, rvu_pe_fac * gpci_pe, rvu_mp * gpci_mp),
      tot_rvu_non = sum(rvu_work * gpci_work, rvu_pe_non * gpci_pe, rvu_mp * gpci_mp),
      par_fee_fac = tot_rvu_fac * conv_fct, # 32.7442
      par_fee_non = tot_rvu_non * conv_fct,
      non_par_fee_fac  = non_participating_fee(par_fee_fac), # * 0.95
      non_par_fee_non = non_participating_fee(par_fee_non),
      limit_charge_fac = limiting_charge(par_fee_fac), # * 1.0925
      limit_charge_non = limiting_charge(par_fee_non)) |>
    cols_fees()
}

cols_fees <- function(df) {

  cols <- c(
    'hcpcs',

    'mac',
    'state',
    'locality',

    'rvu_work',
    'rvu_pe_non',
    'rvu_pe_fac',
    'rvu_mp',
    'conv_fct',

    'gpci_work',
    'gpci_pe',
    'gpci_mp',


    'rvu_total_fac',
    'rvu_total_non',

    'participating_fac',
    'participating_non',

    'nonparticipating_fac',
    'nonparticipating_non',

    'limiting_fac',
    'limiting_non'
  )
  df |>
    dplyr::select(dplyr::any_of(cols))
}
