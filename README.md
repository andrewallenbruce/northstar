
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

Version: 0.0.5

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
get_addons(hcpcs = c("33935", "33924")) |> 
  janitor::remove_empty(which = c("cols", "rows"))
```

    > # A tibble: 9 × 7
    >   hcpcs aoc_type complements       edit_type edit_description     edit_effective
    >   <chr> <chr>    <list>                <int> <chr>                         <int>
    > 1 33924 addon    <tibble [1 × 1]>          1 Only Paid if Primar…           2013
    > 2 33924 addon    <tibble [39 × 1]>         1 Only Paid if Primar…           2013
    > 3 33924 addon    <tibble [2 × 1]>          1 Only Paid if Primar…           2013
    > 4 33924 addon    <tibble [10 × 1]>         1 Only Paid if Primar…           2015
    > 5 33924 addon    <tibble [1 × 1]>          1 Only Paid if Primar…           2016
    > 6 33924 addon    <tibble [3 × 1]>          1 Only Paid if Primar…           2021
    > 7 33935 primary  <tibble [2 × 1]>          1 Only Paid if Primar…           2013
    > 8 33935 primary  <tibble [1 × 1]>          1 Only Paid if Primar…           2015
    > 9 33935 primary  <tibble [3 × 1]>          1 Only Paid if Primar…           2018
    > # ℹ 1 more variable: edit_deleted <int>

### Retrieve MUEs

``` r
search_mue(hcpcs = "33935")
```

    > # A tibble: 2 × 6
    >   hcpcs   mue   mai adjudication                 rationale          service_type
    >   <chr> <int> <int> <chr>                        <chr>              <chr>       
    > 1 33935     1     2 Date of Service Edit: Policy Anatomic Consider… Practitioner
    > 2 33935     1     2 Date of Service Edit: Policy Anatomic Consider… Outpatient …

### Procedure-to-Procedure Edits

``` r
get_ptp_edits(hcpcs = "33935")
```

    > # A tibble: 35 × 6
    >    hcpcs ptp_type      complements      date_deleted edit_mod edit_rationale    
    >    <chr> <chr>         <list>           <date>          <int> <chr>             
    >  1 33935 comprehensive <tibble [1 × 1]> 2004-12-31          1 Standards of medi…
    >  2 33935 comprehensive <tibble [5 × 1]> 2005-12-31          1 Standards of medi…
    >  3 33935 comprehensive <tibble [2 × 1]> 2006-12-31          1 Standards of medi…
    >  4 33935 comprehensive <tibble [2 × 1]> 2007-12-31          1 CPT Separate proc…
    >  5 33935 comprehensive <tibble [5 × 1]> 2008-12-31          1 Standards of medi…
    >  6 33935 comprehensive <tibble [1 × 1]> 2009-04-01          9 Standards of medi…
    >  7 33935 comprehensive <tibble [2 × 1]> 2009-12-31          0 Misuse of Column …
    >  8 33935 comprehensive <tibble [1 × 1]> 2011-12-31          0 HCPCS/CPT procedu…
    >  9 33935 comprehensive <tibble [1 × 1]> 2012-12-31          1 CPT Separate proc…
    > 10 33935 comprehensive <tibble [2 × 1]> 2013-07-01          9 CPT Manual or CMS…
    > # ℹ 25 more rows

``` r
search_ptp(column_1 = "33935") |> 
  janitor::remove_empty(which = c("cols", "rows")) |> 
  dplyr::filter(deletion > clock::date_today("")) |> 
  dplyr::mutate(deletion = NULL) |> 
  dplyr::arrange(column_2) |> 
  dplyr::group_by(column_1, modifier, rationale) |> 
  tidyr::nest() |> 
  dplyr::ungroup()
```

    > # A tibble: 11 × 4
    >    column_1 modifier rationale                                         data    
    >    <chr>       <int> <chr>                                             <list>  
    >  1 33935           0 Misuse of Column Two code with Column One code    <tibble>
    >  2 33935           1 Standards of medical/surgical practice            <tibble>
    >  3 33935           1 CPT Manual or CMS manual coding instruction       <tibble>
    >  4 33935           1 Misuse of Column Two code with Column One code    <tibble>
    >  5 33935           0 HCPCS/CPT procedure code definition               <tibble>
    >  6 33935           1 CPT Separate procedure definition                 <tibble>
    >  7 33935           0 CPT Separate procedure definition                 <tibble>
    >  8 33935           0 Mutually exclusive procedures                     <tibble>
    >  9 33935           0 CPT Manual or CMS manual coding instruction       <tibble>
    > 10 33935           0 Standards of medical/surgical practice            <tibble>
    > 11 33935           0 Anesthesia service included in surgical procedure <tibble>

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
