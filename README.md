
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="250" />

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
  hcpcs    = "33935",
  state    = "GA",
  locality = "01",
  mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 59
    > $ hcpcs                  <chr> "33935"
    > $ description            <chr> "Transplantation heart/lung"
    > $ description_consumer   <chr> "Transplantation of donor heart and lung"
    > $ descriptions_clinician <list> [<tbl_df[1 x 1]>]
    > $ rbcs_category          <chr> "Major Procedure"
    > $ rbcs_family            <chr> "Cardiovascular"
    > $ status                 <chr> "R"
    > $ mac                    <chr> "10212"
    > $ state                  <chr> "GA"
    > $ locality               <chr> "01"
    > $ area                   <chr> "ATLANTA"
    > $ counties               <chr> "BUTTS, CHEROKEE, CLAYTON, COBB, DEKALB, DOUGLA…
    > $ wgpci                  <dbl> 1
    > $ pgpci                  <dbl> 0.997
    > $ mgpci                  <dbl> 1.128
    > $ wrvu                   <dbl> 91.78
    > $ fprvu                  <dbl> 31.07
    > $ mrvu                   <dbl> 21.24
    > $ cf                     <dbl> 32.7442
    > $ f_fee                  <dbl> 4804.08
    > $ nf_fee                 <dbl> 4804.08
    > $ frvus                  <dbl> 146.72
    > $ nrvus                  <dbl> 146.72
    > $ fpar                   <dbl> 4804.23
    > $ npar                   <dbl> 4804.23
    > $ fnpar                  <dbl> 4564.02
    > $ nfnpar                 <dbl> 4564.02
    > $ flim                   <dbl> 5248.62
    > $ nlim                   <dbl> 5248.62
    > $ opps                   <chr> "9"
    > $ opps_nf                <dbl> NA
    > $ opps_f                 <dbl> NA
    > $ fprvu_opps             <dbl> 0
    > $ mrvu_opps              <dbl> 0
    > $ mult_surg              <chr> "2"
    > $ mult_proc              <chr> "2"
    > $ nther                  <dbl> 0
    > $ fther                  <dbl> 0
    > $ global                 <chr> "090"
    > $ op_ind                 <dbl> 1
    > $ op_pre                 <dbl> 0.09
    > $ op_intra               <dbl> 0.84
    > $ op_post                <dbl> 0.07
    > $ mod                    <chr> NA
    > $ pctc                   <chr> "0"
    > $ surg_bilat             <chr> "0"
    > $ surg_asst              <chr> "2"
    > $ surg_co                <chr> "1"
    > $ surg_team              <chr> "2"
    > $ supvis                 <chr> "09"
    > $ dximg                  <chr> "99"
    > $ endo                   <chr> NA
    > $ nfprvu                 <dbl> 31.07
    > $ ntotal                 <dbl> 144.09
    > $ ftotal                 <dbl> 144.09
    > $ nfprvu_opps            <dbl> 0
    > $ two_macs               <lgl> FALSE
    > $ chapter                <chr> "Surgery"
    > $ range                  <chr> "10004 - 69990"

### Retrieve Add-On Codes

``` r
compare_addons(hcpcs = "33935") |> 
  tidyr::unnest(complements) |> 
  janitor::remove_empty(which = c("cols", "rows")) |> 
  dplyr::filter(is.na(edit_deleted))
```

    > # A tibble: 4 × 7
    >   hcpcs aoc_type complement  type type_description   edit_effective edit_deleted
    >   <chr> <chr>    <chr>      <int> <chr>                       <int>        <int>
    > 1 33935 primary  33924          1 Only Paid if Prim…           2015           NA
    > 2 33935 primary  34714          1 Only Paid if Prim…           2018           NA
    > 3 33935 primary  34716          1 Only Paid if Prim…           2018           NA
    > 4 33935 primary  34833          1 Only Paid if Prim…           2018           NA

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
search_ptp(column_1 = "33935") |> 
  janitor::remove_empty(which = c("cols", "rows")) |> 
  dplyr::filter(deletion > clock::date_today("")) |> 
  dplyr::mutate(deletion = NULL) |> 
  dplyr::arrange(column_2) |> 
  dplyr::group_by(column_1, modifier, rationale) |> 
  tidyr::nest()
```

    > # A tibble: 11 × 4
    > # Groups:   column_1, modifier, rationale [11]
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
