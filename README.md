
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="300" />

> Tidy Healthcare Revenue Integrity Tools

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)
<br>
[![Version](https://img.shields.io/badge/version-0.0.4-red.svg)](https://github.com/andrewallenbruce/northstar)
<br> [![Codecov test
coverage](https://codecov.io/gh/andrewallenbruce/northstar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/andrewallenbruce/northstar?branch=master)
<br> [![Code
size](https://img.shields.io/github/languages/code-size/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar)
<br> [![Last
commit](https://img.shields.io/github/last-commit/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar/commits/master)
<br>

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
```

### Search Fee Schedule

``` r
search_fee_schedule(
  hcpcs    = "33935",
  state    = "GA",
  locality = "01",
  mac      = "10212"
)
```

    > # A tibble: 1 × 59
    >   hcpcs description    description_consumer descriptions_clinician rbcs_category
    >   <chr> <chr>          <chr>                <list>                 <chr>        
    > 1 33935 Transplantati… Transplantation of … <tibble [1 × 1]>       Major Proced…
    > # ℹ 54 more variables: rbcs_family <chr>, status <chr>, mac <chr>, state <chr>,
    > #   locality <chr>, area <chr>, counties <chr>, wgpci <dbl>, pgpci <dbl>,
    > #   mgpci <dbl>, wrvu <dbl>, fprvu <dbl>, mrvu <dbl>, cf <dbl>, f_fee <dbl>,
    > #   nf_fee <dbl>, frvus <dbl>, nrvus <dbl>, fpar <dbl>, npar <dbl>,
    > #   fnpar <dbl>, nfnpar <dbl>, flim <dbl>, nlim <dbl>, opps <chr>,
    > #   opps_nf <dbl>, opps_f <dbl>, fprvu_opps <dbl>, mrvu_opps <dbl>,
    > #   mult_surg <chr>, mult_proc <chr>, nther <dbl>, fther <dbl>, global <chr>, …

### Search ICD-10-CM

``` r
icd10cm(code = "T38.0X1A")
```

    > # A tibble: 1 × 16
    >      ch abbrev chapter  range ch_start ch_end ch_codes ch_sc section order valid
    >   <int> <chr>  <chr>    <chr>    <int>  <int>    <int> <int> <chr>   <int> <int>
    > 1    19 INJ    Injury,… S00 …    30990  84933    53943   171 Poison… 75443     1
    > # ℹ 5 more variables: code <chr>, description <chr>, sc_start <int>,
    > #   sc_end <int>, sc_codes <int>

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
