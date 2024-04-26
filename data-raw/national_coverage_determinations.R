source(here::here("data-raw", "file_paths.R"))
source(here::here("data-raw", "load_packages.R"))
source(here::here("data-raw", "pins_functions.R"))

ncd <- ncd_paths |> purrr::map(read_csv, col_types = "c")

# NCD_BNFT_CTGRY_REF
#
# bnft_ctgry_cd Benefit Category Code
# bnft_ctgry_desc Benefit Category Description
#
# Primary Keys: bnft_ctgry_cd
#
# Usage: This table contains the universe of Medicare Benefit Category reference values.
# Join from table NCD_TRKG_BNFT_XREF on benefit_category_cd to retrieve the description for a benefit category.


# NCD_PBLCTN_REF
#
# pblctn_cd Internal ID
# pblctn_num Manual Publication Number
# pblctn_title Benefit Category Title
#
# Primary Keys: pblctn_cd
#
# Usage: This table contains the universe of Publication reference values.
# Join from table NCD_TRKG pblctn_cd to retrieve the description for a publication.


# NCD_TRKG
#
# Primary Keys: NCD_id, NCD_vrsn_num
#
# Usage: This is the primary NCD table and contains one record for each version of an NCD.

# NCD_TRKG_BNFT_XREF

benefit_cat <- ncd$ncd_trkg_bnft_xref |>
  mutate(bnft_ctgry_cd = as.character(bnft_ctgry_cd)) |>
  left_join(ncd$ncd_bnft_ctgry_ref)

pub_ref <- ncd$ncd_pblctn_ref |>
  mutate(pblctn_cd = as.character(pblctn_cd))

ncd_join <- ncd$ncd_trkg |>
  mutate(pblctn_cd = as.character(pblctn_cd)) |>
  left_join(benefit_cat) |>
  left_join(pub_ref) |>
  clean_names() |>
  mutate(
    natl_cvrg_type     = case_match(natl_cvrg_type, TRUE ~ "NCD", FALSE ~ "Coverage Provision"),
    xref_txt           = iconv(xref_txt, to = "ASCII//TRANSLIT"),
    itm_srvc_desc      = iconv(itm_srvc_desc, to = "ASCII//TRANSLIT"),
    indctn_lmtn        = iconv(indctn_lmtn, to = "ASCII//TRANSLIT"),
    othr_txt           = iconv(othr_txt, to = "ASCII//TRANSLIT"),
    trnsmtl_url        = iconv(trnsmtl_url, to = "ASCII//TRANSLIT"),
    rev_hstry          = iconv(rev_hstry, to = "ASCII//TRANSLIT"),
    ncd_keyword        = iconv(ncd_keyword, to = "ASCII//TRANSLIT"),
    ncd_mnl_sect_title = iconv(ncd_mnl_sect_title, to = "ASCII//TRANSLIT"),
    ncd_efctv_dt       = as.character(ncd_efctv_dt),
    ncd_impltn_dt      = as.character(ncd_impltn_dt),
    ncd_trmntn_dt      = as.character(ncd_trmntn_dt),
    trnsmtl_issnc_dt   = as.character(trnsmtl_issnc_dt),
    creatd_tmstmp      = as.character(creatd_tmstmp),
    last_updt_tmstmp   = as.character(last_updt_tmstmp),
    last_clrnc_tmstmp  = as.character(last_clrnc_tmstmp)
    ) |>
  select(
    id             = ncd_id,
    version        = ncd_vrsn_num,
    # cov_type       = natl_cvrg_type,
    coverage       = cvrg_lvl_cd,
    section        = ncd_mnl_sect,
    section_name   = ncd_mnl_sect_title,
    date_eff       = ncd_efctv_dt,
    date_impl      = ncd_impltn_dt,
    date_end       = ncd_trmntn_dt,
    description    = itm_srvc_desc,
    indications    = indctn_lmtn,
    crossref       = xref_txt,
    other          = othr_txt,
    trsm_no        = trnsmtl_num,
    trsm_date      = trnsmtl_issnc_dt,
    trsm_url       = trnsmtl_url,
    chg_req        = chg_rqst_num,
    review_history = rev_hstry,
    under_review   = under_rvw,
    date_created   = creatd_tmstmp,
    last_updated   = last_updt_tmstmp,
    date_approved  = last_clrnc_tmstmp,
    lab            = ncd_lab,
    keyword        = ncd_keyword,
    ama_copyright  = ncd_ama,
    benefit_code   = bnft_ctgry_cd,
    benefit_cat    = bnft_ctgry_desc,
    pub_code       = pblctn_cd,
    pub_number     = pblctn_num,
    pub_title      = pblctn_title) |>
  mutate(
    chg_req       = as.character(chg_req),
    trsm_no       = as.character(trsm_no),
    version       = as.character(version),
    coverage      = as.character(coverage),
    coverage      = case_match(coverage,
      "1" ~ "Full",
      "2" ~ "Restricted",
      "3" ~ "None",
      "4" ~ "Unknown"),
    date_eff      = lubridate::ymd(date_eff),
    date_eff      = as.Date(date_eff),
    date_impl     = lubridate::ymd(date_impl),
    date_end      = lubridate::ymd(date_end),
    trsm_date     = lubridate::ymd(trsm_date),
    date_approved = strex::str_before_first(date_approved, " "),
    date_approved = lubridate::ymd(date_approved),
    date_created  = strex::str_before_first(date_created, " "),
    date_created  = lubridate::ymd(date_created),
    last_updated  = strex::str_before_first(last_updated, " "),
    last_updated  = lubridate::ymd(last_updated)
    )

# Update Pin
pin_update(
  ncd_join,
  name = "ncd",
  title = "NCD Download Database. Last Updated 2022-12-08"
)

ncd_join |>
  select(contains("date")) |>
  count(date_eff) |>
  print(n = Inf)

ncd_join |>
  select(
    id,
    version,
    coverage,
    section,
    section_name,
    keyword,
    description,
    indications,
    # crossref,
    # other,
    # trsm_no,
    # trsm_url,
    review_history,
    # date_eff,
    # date_impl,
    # date_end,
    # date_created,
    # last_updated,
    # date_approved,
    # trsm_date
    ) |>
  gt() |>
  sub_missing(missing_text = "") |>
  # fmt_date(columns = everything()) |>
  fmt_markdown() |>
  opt_table_font(font = google_font(name = "Rubik")) |>
  tab_options(
    table.font.size =  px(14),
    table.width = pct(75))


ncd_join |>
  select(
    date_eff,
    date_impl,
    date_end,
    date_created,
    last_updated,
    date_approved,
    trsm_date) |>
  gt() |>
  sub_missing(missing_text = "") |>
  fmt_date(columns = everything())
