#------------ vector of unique CPT codes
cpt_vec <- get_pin("cpt_descriptors") |>
  dplyr::count(hcpcs, sort = TRUE) |>
  dplyr::pull(hcpcs)

length(cpt_vec) # 10641


#------------ vector of unique HCPCS codes
hcpcs_vec <- get_pin("hcpcs") |>
  dplyr::filter(type == "code") |>
  dplyr::count(hcpcs, sort = TRUE) |>
  dplyr::pull(hcpcs)

length(hcpcs_vec) # 7902

#------------ vector of unique RBCS codes
rbcs_vec <- get_pin("rbcs") |>
  dplyr::count(hcpcs, sort = TRUE) |>
  dplyr::pull(hcpcs)

length(rbcs_vec) # 15522

#------------ 0 codes that are both cpt and hcpcs
vctrs::vec_set_intersect(cpt_vec, hcpcs_vec)

#------------ 10641 codes that are cpt only
length(vctrs::vec_set_difference(cpt_vec, hcpcs_vec))

#------------ 7902 codes that are hcpcs only
length(vctrs::vec_set_difference(hcpcs_vec, cpt_vec))

#------------ TOTAL 18543 codes that are either cpt or hcpcs
length(vctrs::vec_set_union(cpt_vec, hcpcs_vec))

hcpcs_vecs <- list(
  cpt   = cpt_vec,   # 10641
  hcpcs = hcpcs_vec  # 7902
)

sum(10641, 7902, 15522) # 34065

#------------ 9878 codes that are both cpt and rbcs
rbcs_cpt <- vctrs::vec_set_intersect(cpt_vec, rbcs_vec)
length(rbcs_cpt) + length(rbcs_hcpcs)

#------------ 4511 codes that are both hcpcs and rbcs
rbcs_hcpcs <- vctrs::vec_set_intersect(hcpcs_vec, rbcs_vec)
length(rbcs_hcpcs)

#------------ 14389 codes that are rbcs and either cpt or hcpcs
length(rbcs_cpt) + length(rbcs_hcpcs)

#------------ 1133 codes that are only rbcs
rbcs_only <- vctrs::vec_set_difference(rbcs_vec, vctrs::vec_c(rbcs_cpt, rbcs_hcpcs))
length(rbcs_vec) - (length(rbcs_cpt) + length(rbcs_hcpcs))

#------------ TOTAL 15522 codes
length(rbcs_only) + length(rbcs_cpt) + length(rbcs_hcpcs)

sum(9878, 4511, 1133) # 15522
length(rbcs_vec) # 15522

rbcs_vecs <- list(
  rbcs_cpt = rbcs_cpt,     # 9878
  rbcs_hcpcs = rbcs_hcpcs, # 4511
  rbcs_only  = rbcs_only   # 1133
)

# Update Pin
board <- pins::board_folder(here::here("inst/extdata/pins"))

board |>
  pins::pin_write(
    hcpcs_vecs,
    name        = "cpt_hcpcs_vecs",
    title       = "HCPCS Code Vectors",
    description = "List of 2 Vectors of Unique Codes: CPT (10641) & HCPCS (7902), a total of 18543 codes.",
    type = "qs"
  )

board |>
  pins::pin_write(
    rbcs_vecs,
    name        = "rbcs_vecs",
    title       = "RBCS Code Vectors",
    description = "List of 3 Vectors of Unique Codes: RBCS and CPT, RBCS and HCPCS, and RBCS Only, a total of 15522 codes.",
    type = "qs"
  )

board |> pins::write_board_manifest()
