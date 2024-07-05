.as <- list(
  chr = \(...) as.character(...),
  int = \(...) as.integer(...),
  dbl = \(...) as.double(...),
  lgl = \(...) as.logical(...),
  fct = \(...) as.factor(...),
  dte = \(...) as.Date(...),
  vct = \(...) as.vector(...)
)


# .sets <- new.env()
#
# .sets$ncci <- list(
#   aoc = get_pin("ncci_aoc_nested"),
#   mue = get_pin("ncci_mue_prac"),
#   ptp = get_pin("ncci_ptp_prac")
#   )

# .sets$ncci$aoc
#
# .pin <- list(
#   adj = get_pin("adj_codes"),
#   aoc = get_pin("ncci_aoc_nested"),
#   mue = get_pin("ncci_mue_prac"),
#   ptp = get_pin("ncci_ptp_prac"),
#   rvu = \(type) switch(type,
#                        amt = get_pin("pfs_rvu_amt"),
#                        ind = get_pin("pfs_rvu_ind")),
#   gpci = get_pin("pfs_gpci"),
#   desc = get_pin("hcpcs_descriptions"),
#   mod  = get_pin("modifiers"),
#   hcpcs = get_pin("hcpcs_lvl2"),
#   pos = get_pin("pos_codes"),
#   rbcs = get_pin("rbcs")
#   )
