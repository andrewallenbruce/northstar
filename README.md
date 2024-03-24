
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="150" />

> Tidy Healthcare Revenue Integrity Tools

<!-- badges: start -->

    > 
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![Codecov test coverage](https://codecov.io/gh/andrewallenbruce/northstar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/andrewallenbruce/northstar?branch=master) |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)          |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![Code size](https://img.shields.io/github/languages/code-size/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar)                                |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![Last commit](https://img.shields.io/github/last-commit/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar/commits/master)                       |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![License: Apache License (>= 2)](https://img.shields.io/badge/license-Apache License (>= 2)-blue.svg)](https://cran.r-project.org/web/licenses/Apache License (>= 2)         |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
    > | [![Version](https://img.shields.io/badge/devel%20version-0.0.2-red.svg)](https://github.com/andrewallenbruce/northstar)                                                        |
    > +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

[![Codecov test
coverage](https://codecov.io/gh/andrewallenbruce/northstar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/andrewallenbruce/northstar?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)
[![Code
size](https://img.shields.io/github/languages/code-size/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar)
[![Last
commit](https://img.shields.io/github/last-commit/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar/commits/master)
[![License: Apache License (\>=
2)](https://img.shields.io/badge/license-Apache%20License%20(%3E=%202)-blue.svg)](https://cran.r-project.org/web/licenses/Apache%20License%20(%3E=%202))
[![Version](https://img.shields.io/badge/devel%20version-0.0.2-red.svg)](https://github.com/andrewallenbruce/northstar)
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

## Search HCPCS Codes

``` r
hcpcs_search(hcpcs    = "33935", 
             state    = "GA", 
             locality = "01", 
             mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 54
    > $ hcpcs                 <chr> "33935"
    > $ level                 <chr> "1"
    > $ category              <chr> "1"
    > $ description           <chr> "Transplantation heart/lung"
    > $ description_consumer  <chr> "Transplantation of donor heart and lung"
    > $ description_clinician <list> [<tbl_df[1 x 1]>]
    > $ section               <chr> "Surgery [10004-69990]"
    > $ rbcs_category         <chr> "Procedure"
    > $ rbcs_subcategory      <chr> "Cardiovascular"
    > $ rbcs_family           <chr> "No RBCS Family"
    > $ rbcs_procedure        <chr> "Major"
    > $ status                <chr> "Bundled/Excluded Code"
    > $ mac                   <chr> "10212"
    > $ state                 <chr> "GA"
    > $ locality              <chr> "01"
    > $ area                  <chr> "ATLANTA"
    > $ wgpci                 <dbl> 1
    > $ pgpci                 <dbl> 0.997
    > $ mgpci                 <dbl> 1.128
    > $ wrvu                  <dbl> 91.78
    > $ nonfac_prvu           <dbl> 31.07
    > $ fac_prvu              <dbl> 31.07
    > $ mrvu                  <dbl> 21.24
    > $ cf                    <dbl> 32.744
    > $ fac_par               <dbl> 4804.053
    > $ nonfac_par            <dbl> 4804.053
    > $ fac_nonpar            <dbl> 4563.85
    > $ nonfac_nonpar         <dbl> 4563.85
    > $ fac_limit             <dbl> 5248.428
    > $ nonfac_limit          <dbl> 5248.428
    > $ opps                  <chr> "9"
    > $ nonfac_prvu_opps      <dbl> 0
    > $ fac_prvu_opps         <dbl> 0
    > $ mrvu_opps             <dbl> 0
    > $ mult_surg             <chr> "0"
    > $ mult_proc             <chr> "2"
    > $ flat_vis              <dbl> 0
    > $ nonfac_ther           <dbl> 2
    > $ fac_ther              <dbl> 0
    > $ global                <chr> "090"
    > $ op_pre                <dbl> 0.09
    > $ op_intra              <dbl> 0.84
    > $ op_post               <dbl> 0.07
    > $ mod                   <chr> NA
    > $ pctc                  <chr> "0"
    > $ surg_bilat            <chr> "0"
    > $ surg_asst             <chr> "2"
    > $ surg_co               <chr> "1"
    > $ surg_team             <chr> "2"
    > $ supvis                <chr> "09"
    > $ dximg                 <chr> "99"
    > $ endo                  <chr> NA
    > $ rare                  <chr> "10"
    > $ unused                <int> 0

## Search ICD-10 Codes

``` r
icd10_search(code  = "T38.0X1A", 
             field = "code") |> 
  case_section_icd10(code) |> 
  dplyr::glimpse()
```

    > Rows: 1
    > Columns: 3
    > $ code        <chr> "T38.0X1A"
    > $ section     <chr> "Injury, Poisoning and Certain Other Consequences of Exter…
    > $ description <chr> "Poisoning by glucocorticoids and synthetic analogues, acc…

## Physician Fee Schedule Calculation

``` r
calculate_amounts(
  wrvu  = 6.26,
  nprvu = 7.92,
  fprvu = 4.36,
  mrvu  = 0.99,
  cf    = 32.744,
  wgpci = 1,
  pgpci = 0.883,
  mgpci = 1.125
)
```

    > Facility Amounts:
    > 
    > RVU Total ............ 11.22
    > Participating ........ $367.51
    > Non-Particpating ..... $349.13
    > Limiting Charge ...... $401.50
    > 
    > Non-Facility Amounts:
    > 
    > RVU Total ............ 14.37
    > Participating ........ $470.44
    > Non-Particpating ..... $446.91
    > Limiting Charge ...... $513.95

------------------------------------------------------------------------

## :balance_scale: Code of Conduct

Please note that the `northstar` project is released with a [Contributor
Code of
Conduct](https://andrewallenbruce.github.io/northstar/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

<br>

## :classical_building: Governance

This project is primarily maintained by [Andrew
Bruce](https://github.com/andrewallenbruce). Other authors may
occasionally assist with some of these duties.
