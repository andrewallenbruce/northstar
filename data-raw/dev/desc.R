# EDA ------------------
cpt_uq <- collapse::funique(cpt_desc$hcpcs) # 10641
two_uq <- collapse::funique(two_desc$hcpcs) # 7966
rvu_uq <- collapse::funique(rvu_desc$hcpcs) # 16325

length(c(rvu_uq, cpt_uq, two_uq)) # 34932

# Neither are in the other group - Total unique 18607
cpt_two_uq <- c(cpt_uq, two_uq)

# RVU & CPT - 10557
vctrs::vec_set_intersect(rvu_uq, cpt_uq) |> length()
# CPT only - 84
cpt_only <- vctrs::vec_set_difference(cpt_uq, rvu_uq)

cpt_desc |>
  filter(hcpcs %in% cpt_only)

# RVU & HCPCS - 4793
vctrs::vec_set_intersect(rvu_uq, two_uq) |> length()
# HCPCS only - 3173
hcpcs_only <- vctrs::vec_set_difference(two_uq, rvu_uq)

vctrs::vec_set_difference(rvu_uq, cpt_uq) |> length() # 5768
vctrs::vec_set_difference(rvu_uq, two_uq) |> length() # 11532

# RVU Only - 975
rvu_only <- vctrs::vec_set_difference(rvu_uq, cpt_two_uq)

rvu_desc |>
  filter(hcpcs %in% rvu_only)
