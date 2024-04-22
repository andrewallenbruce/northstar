
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="225" />

> Tidy Healthcare Revenue Integrity

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)
[![Version](https://img.shields.io/badge/version-0.0.5-red.svg)](https://github.com/andrewallenbruce/northstar)
[![Code
size](https://img.shields.io/github/languages/code-size/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar)
[![Codecov test
coverage](https://codecov.io/gh/andrewallenbruce/northstar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/andrewallenbruce/northstar?branch=master)
[![Last
commit](https://img.shields.io/github/last-commit/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar/commits/master)
<!-- badges: end -->

<br>

## :package: Installation

You can install **northstar** from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("andrewallenbruce/northstar")
```

## :beginner: Usage

``` r
library(northstar)
library(dplyr)
library(tidyr)
library(janitor)
```

### Search Fee Schedule

``` r
search_fee_schedule(
  hcpcs    = "33924",
  state    = "GA",
  locality = "01",
  mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 62
    > $ hcpcs          <chr> "33924"
    > $ description    <chr> "Remove pulmonary shunt"
    > $ rbcs_category  <chr> "Major Procedure"
    > $ rbcs_family    <chr> "Cardiovascular"
    > $ status         <chr> "A"
    > $ mac            <chr> "10212"
    > $ state          <chr> "GA"
    > $ locality       <chr> "01"
    > $ area           <chr> "ATLANTA"
    > $ counties       <chr> "BUTTS, CHEROKEE, CLAYTON, COBB, DEKALB, DOUGLAS, FAYET…
    > $ wgpci          <dbl> 1
    > $ pgpci          <dbl> 0.997
    > $ mgpci          <dbl> 1.128
    > $ wrvu           <dbl> 5.49
    > $ fprvu          <dbl> 1.5
    > $ mrvu           <dbl> 1.35
    > $ cf             <dbl> 32.7442
    > $ f_fee          <dbl> 278.6
    > $ nf_fee         <dbl> 278.6
    > $ frvus          <dbl> 8.51
    > $ nrvus          <dbl> 8.51
    > $ fpar           <dbl> 278.65
    > $ npar           <dbl> 278.65
    > $ fnpar          <dbl> 264.72
    > $ nfnpar         <dbl> 264.72
    > $ flim           <dbl> 304.43
    > $ nlim           <dbl> 304.43
    > $ opps           <chr> "9"
    > $ opps_nf        <dbl> NA
    > $ opps_f         <dbl> NA
    > $ fprvu_opps     <dbl> 0
    > $ mrvu_opps      <dbl> 0
    > $ mult_surg      <chr> "0"
    > $ mult_proc      <chr> "0"
    > $ nther          <dbl> 0
    > $ fther          <dbl> 0
    > $ global         <chr> "ZZZ"
    > $ op_ind         <dbl> 0
    > $ op_pre         <dbl> 0
    > $ op_intra       <dbl> 0
    > $ op_post        <dbl> 0
    > $ mod            <chr> NA
    > $ pctc           <chr> "0"
    > $ surg_bilat     <chr> "0"
    > $ surg_asst      <chr> "2"
    > $ surg_co        <chr> "1"
    > $ surg_team      <chr> "0"
    > $ supvis         <chr> "09"
    > $ dximg          <chr> "99"
    > $ endo           <chr> NA
    > $ nfprvu         <dbl> 1.5
    > $ ntotal         <dbl> 8.34
    > $ ftotal         <dbl> 8.34
    > $ nfprvu_opps    <dbl> 0
    > $ two_macs       <lgl> FALSE
    > $ cpt_chapter    <chr> "Surgery"
    > $ cpt_range      <chr> "10004 - 69990"
    > $ cpt_desc_cons  <chr> "Disconnection of pulmonary artery shunt"
    > $ cpt_desc_long  <chr> "Ligation and takedown of a systemic-to-pulmonary arter…
    > $ cpt_desc_short <chr> "REMOVE PULMONARY SHUNT"
    > $ cpt_desc_med   <chr> "LIG&TKDN SYSIC-TO-PULM ART SHUNT W/CGEN HEART"
    > $ cpt_desc_clin  <list> [<tbl_df[2 x 1]>]

### Retrieve Add-On Codes

``` r
get_addon_edits(hcpcs = "33924") |> 
  dplyr::filter(is.na(aoc_edit_deleted), 
                is.na(aoc_year_deleted)) |> 
  janitor::remove_empty(which = c("cols", "rows"))
```

    > # A tibble: 4 × 6
    >   hcpcs aoc_type aoc_complements   aoc_edit_type aoc_edit_description           
    >   <chr> <chr>    <list>                    <int> <chr>                          
    > 1 33924 addon    <tibble [39 × 1]>             1 Only Paid if Primary is Paid. …
    > 2 33924 addon    <tibble [10 × 1]>             1 Only Paid if Primary is Paid. …
    > 3 33924 addon    <tibble [1 × 1]>              1 Only Paid if Primary is Paid. …
    > 4 33924 addon    <tibble [3 × 1]>              1 Only Paid if Primary is Paid. …
    > # ℹ 1 more variable: aoc_edit_effective <int>

### Retrieve MUEs

``` r
get_mue_edits(hcpcs = "33935", 
              service_type = "Practitioner") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 6
    > $ hcpcs            <chr> "33935"
    > $ mue_uos          <int> 1
    > $ mue_mai          <int> 2
    > $ mue_mai_desc     <chr> "Date of Service Edit: Policy"
    > $ mue_service_type <chr> "Practitioner"
    > $ mue_rationale    <chr> "Anatomic Consideration"

### Procedure-to-Procedure Edits

``` r
get_ptp_edits(hcpcs = "33935", 
              ptp_edit_mod = 1) |> 
  dplyr::filter(ptp_deleted > clock::date_today(""))
```

    > # A tibble: 4 × 7
    >   hcpcs ptp_type      ptp_complements ptp_deleted ptp_edit_mod ptp_edit_mod_desc
    >   <chr> <chr>         <list>          <date>             <int> <chr>            
    > 1 33935 comprehensive <tibble>        9999-12-31             1 Allowed          
    > 2 33935 comprehensive <tibble>        9999-12-31             1 Allowed          
    > 3 33935 comprehensive <tibble>        9999-12-31             1 Allowed          
    > 4 33935 comprehensive <tibble>        9999-12-31             1 Allowed          
    > # ℹ 1 more variable: ptp_edit_rationale <chr>

------------------------------------------------------------------------

## :balance_scale: Code of Conduct

Please note that the `northstar` project is released with a [Contributor
Code of
Conduct](https://andrewallenbruce.github.io/northstar/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## :classical_building: Governance

This project is primarily maintained by [Andrew
Bruce](https://github.com/andrewallenbruce). Other authors may
occasionally assist with some of these duties.
