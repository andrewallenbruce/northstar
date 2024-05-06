source(here::here("data-raw", "load_packages.R"))

cpt_desc <- get_pin("cpt_descriptors")
two_desc <- get_pin("two_descriptions")
rvu_desc <- get_pin("hcpcs_rvu_descriptions")

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

rvu_clean <- rvu_desc |>
  filter(hcpcs != "\032") |>
  rename(description = rel_desc) |>
  mutate(desc_type = "Short")

two_pivot <- two_desc |>
  select(
    hcpcs,
    Short = two_desc_short,
    Long = two_desc_long
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

cpt_pivot <- cpt_desc |>
  unnest(cpt_desc_clin) |>
  select(
    hcpcs,
    Short = cpt_desc_short,
    Long = cpt_desc_long,
    Medical = cpt_desc_med,
    Consumer = cpt_desc_cons,
    Clinician = cpt_desc_clin
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

hcpcs_desc <- rvu_clean |>
  dplyr::full_join(two_pivot) |>
  dplyr::full_join(cpt_pivot) |>
  dplyr::arrange(hcpcs) |>
  dplyr::distinct(hcpcs, description, .keep_all = TRUE)

short_dupes <- hcpcs_desc |>
  dplyr::filter(desc_type == "Short") |>
  janitor::get_dupes(hcpcs, desc_type) |>
  dplyr::count(hcpcs) |>
  dplyr::pull(hcpcs)

desc_dupes <- hcpcs_desc |>
  filter(hcpcs %in% short_dupes) |>
  filter(desc_type == "Short") |>
  group_by(hcpcs) |>
  slice_tail() |>
  ungroup() |>
  pull(description)

hcpcs_desc <- hcpcs_desc |>
  filter(!description %in% desc_dupes)

# Sanity check --------------------------
hcpcs_desc |>
  count(hcpcs) |>
  select(hcpcs) |>
  # filter(hcpcs %in% cpt_uq) # 10641
  # filter(hcpcs %in% two_uq) # 7966
  filter(hcpcs %in% rvu_uq) # 16324
# ---------------------------------------

# Add CPT PLA Codes
library(codexchain)

cpt_pla_codes <- codexchain::placodes |>
  select(
    hcpcs = X1,
    desc_long = X2,
    desc_medium = X3,
    desc_short = X4,
    X5,
    X6,
    date_released = X7,
    date_effective = X8,
    proprietary_name = X9,
    clinical_lab = X10,
    manufacturer = X11
  ) |>
  janitor::remove_empty()

pla_pivot <- cpt_pla_codes |>
  select(
    hcpcs,
    Short = desc_short,
    Medium = desc_medium,
    Long = desc_long,
    "Proprietary Name" = proprietary_name
  ) |>
  pivot_longer(
    !hcpcs,
    names_to = "desc_type",
    values_to = "description")

hcpcs_desc <- vctrs::vec_rbind(
  hcpcs_desc,
  pla_pivot
)

pin_update(
  hcpcs_desc,
  name = "hcpcs_descriptions",
  title = "HCPCS Descriptions Master File",
  description = "HCPCS Descriptions Master File"
)

# A tibble: 5 × 2
# end_letter     n
# 1 A            142
# 2 F            565
# 3 M             65
# 4 T           2224
# 5 U           1348
hcpcs_desc |>
  filter(str_ends(hcpcs, regex("[A-Z]"))) |>
  mutate(end_letter = str_extract(hcpcs, regex("[A-Z]"))) |>
  count(end_letter) |>
  print(n = Inf)

hcpcs_desc |>
  dplyr::rowwise() |>
  dplyr::mutate()

is_level_I("0001A")
is_category_I("0001A")

"0001A" %in% cpt_only

is_category_II("0001F")

?is_category_II

cpt_desc |>
  select(
    hcpcs,
    chapter = cpt_chapter,
    range = cpt_range
  )

two_desc
