source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "pins_functions.R"))

cpt_desc <- get_pin("cpt_descriptors")
two_desc <- get_pin("two_descriptions")
rvu_desc <- get_pin("hcpcs_rvu_descriptions")

rvu_short <- rvu_desc |>
  rename(description = rel_desc) |>
  mutate(desc_type = "Short")

two_short <- two_desc |>
  select(hcpcs,
         two_desc_short) |>
  rename(description = two_desc_short) |>
  mutate(desc_type = "Short")

two_long <- two_desc |>
  select(hcpcs,
         two_desc_long) |>
  rename(description = two_desc_long) |>
  mutate(desc_type = "Long")


two_both <- vctrs::vec_rbind(
  two_short,
  two_long
)

cpt_short <- cpt_desc |>
  select(hcpcs,
         cpt_desc_short) |>
  rename(description = cpt_desc_short) |>
  mutate(desc_type = "Short")

cpt_long <- cpt_desc |>
  select(hcpcs,
         cpt_desc_long) |>
  rename(description = cpt_desc_long) |>
  mutate(desc_type = "Long")

cpt_cons <- cpt_desc |>
  select(hcpcs,
         cpt_desc_cons) |>
  rename(description = cpt_desc_cons) |>
  mutate(desc_type = "Consumer")

cpt_med <- cpt_desc |>
  select(hcpcs,
         cpt_desc_med) |>
  rename(description = cpt_desc_med) |>
  mutate(desc_type = "Medical")

cpt_clin <- cpt_desc |>
  select(hcpcs,
         cpt_desc_clin) |>
  unnest(cpt_desc_clin) |>
  rename(description = cpt_desc_clin) |>
  mutate(desc_type = "Clinician")

cpt_all <- vctrs::vec_rbind(
  cpt_short,
  cpt_long,
  cpt_cons,
  cpt_med,
  cpt_clin
)

uni_cpts <- collapse::funique(cpt_all$hcpcs)

length(uni_cpts)

rvu_short |>
  filter(hcpcs %in% uni_cpts)

rvu_hcpcs <- vctrs::vec_rbind(
  rvu_short,
  two_both
) |>
  distinct()

vctrs::vec_rbind(
  rvu_hcpcs,
  cpt_all
) |>
  nest(description = c(description)) |>
  filter(desc_type == "Clinician") |>
  print(n = 200)
