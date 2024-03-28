
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="250" />

> Tidy Healthcare Revenue Integrity

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)
[![Version](https://img.shields.io/badge/version-0.0.4-red.svg)](https://github.com/andrewallenbruce/northstar)
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

### Search ICD-10-CM

``` r
icd10cm(code = "T38.0X1A") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 10
    > $ ch            <int> 19
    > $ abb           <chr> "INJ"
    > $ chapter_name  <chr> "Injury, poisoning and certain other consequences of ext…
    > $ chapter_range <chr> "S00 - T88"
    > $ section_name  <chr> "Poisoning by, adverse effect of and underdosing of horm…
    > $ section_range <chr> "T38 - T38.996S"
    > $ order         <int> 75443
    > $ valid         <int> 1
    > $ code          <chr> "T38.0X1A"
    > $ description   <chr> "Poisoning by glucocorticoids and synthetic analogues, a…

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
