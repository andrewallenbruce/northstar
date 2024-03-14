library(tidyverse)
library(janitor)
library(gt)

paths <- fs::dir_ls("C:/Users/Andrew/Desktop/all_data/ncd/ncd_csv/", regexp = "*.csv$")
names <- paths |> basename() |> str_remove_all(pattern = fixed(".csv"))
names(paths) <- names
ncd <- paths |> purrr::map(read_csv, col_types = "c")


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

ncd$ncd_trkg |>
  mutate(pblctn_cd = as.character(pblctn_cd)) |>
  left_join(benefit_cat) |>
  left_join(pub_ref) |>
  clean_names() |>
  mutate(
    natl_cvrg_type    = case_match(natl_cvrg_type, TRUE ~ "NCD", FALSE ~ "Coverage Provision"),
    xref_txt          = iconv(xref_txt, to = "ASCII//TRANSLIT"),
    itm_srvc_desc     = iconv(itm_srvc_desc, to = "ASCII//TRANSLIT"),
    indctn_lmtn       = iconv(indctn_lmtn, to = "ASCII//TRANSLIT"),
    othr_txt          = iconv(othr_txt, to = "ASCII//TRANSLIT"),
    trnsmtl_url       = iconv(trnsmtl_url, to = "ASCII//TRANSLIT"),
    rev_hstry         = iconv(rev_hstry, to = "ASCII//TRANSLIT"),
    ncd_efctv_dt      = as.character(ncd_efctv_dt),
    ncd_impltn_dt     = as.character(ncd_impltn_dt),
    ncd_trmntn_dt     = as.character(ncd_trmntn_dt),
    trnsmtl_issnc_dt  = as.character(trnsmtl_issnc_dt),
    creatd_tmstmp     = as.character(creatd_tmstmp),
    last_updt_tmstmp  = as.character(last_updt_tmstmp),
    last_clrnc_tmstmp = as.character(last_clrnc_tmstmp)
    ) |>
  select(
    id             = ncd_id,
    version        = ncd_vrsn_num,
    cov_type       = natl_cvrg_type,
    cov_level      = cvrg_lvl_cd,
    sec_no         = ncd_mnl_sect,
    sec_nm         = ncd_mnl_sect_title,
    date_eff       = ncd_efctv_dt,
    date_impl      = ncd_impltn_dt,
    date_end       = ncd_trmntn_dt,
    service_des    = itm_srvc_desc,
    indic_limit    = indctn_lmtn,
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
  # select(
  #   service_des,
  #   crossref,
  #   indic_limit,
  #   other,
  #   trsm_url,
  #   review_history,
  #   date_eff
  #   ) |>
  gt() |>
  sub_missing(missing_text = "")

