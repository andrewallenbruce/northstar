
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
```

### Describe HCPCS

``` r
describe_hcpcs(hcpcs = "33924")
```

    > # A tibble: 6 × 8
    >   hcpcs description   desc_type level category section rbcs_category rbcs_family
    >   <chr> <chr>         <chr>     <chr> <chr>    <chr>   <chr>         <chr>      
    > 1 33924 Remove pulmo… Short     I     I        Surgery Major Proced… Cardiovasc…
    > 2 33924 Ligation and… Long      I     I        Surgery Major Proced… Cardiovasc…
    > 3 33924 LIG&TKDN SYS… Medical   I     I        Surgery Major Proced… Cardiovasc…
    > 4 33924 Disconnectio… Consumer  I     I        Surgery Major Proced… Cardiovasc…
    > 5 33924 Ligation and… Clinician I     I        Surgery Major Proced… Cardiovasc…
    > 6 33924 Ligation and… Clinician I     I        Surgery Major Proced… Cardiovasc…

### Add-On Codes

``` r
get_addon_edits(hcpcs = "33924", current = TRUE)
```

    > # A tibble: 4 × 9
    >   hcpcs aoc_type aoc_complements   aoc_edit_type aoc_edit_description           
    >   <chr> <chr>    <list>                    <int> <chr>                          
    > 1 33924 addon    <tibble [39 × 1]>             1 Only Paid if Primary is Paid. …
    > 2 33924 addon    <tibble [10 × 1]>             1 Only Paid if Primary is Paid. …
    > 3 33924 addon    <tibble [1 × 1]>              1 Only Paid if Primary is Paid. …
    > 4 33924 addon    <tibble [3 × 1]>              1 Only Paid if Primary is Paid. …
    > # ℹ 4 more variables: aoc_year_deleted <int>, aoc_edit_effective <int>,
    > #   aoc_edit_deleted <int>, aoc_notes <chr>

### Retrieve MUEs

``` r
get_mue_edits(hcpcs = "33935")
```

    > # A tibble: 2 × 6
    >   hcpcs mue_uos mue_mai mue_mai_desc              mue_service_type mue_rationale
    >   <chr>   <int>   <int> <chr>                     <chr>            <chr>        
    > 1 33935       1       2 Date of Service Edit: Po… Practitioner     Anatomic Con…
    > 2 33935       1       2 Date of Service Edit: Po… Outpatient Hosp… Anatomic Con…

### Procedure-to-Procedure Edits

``` r
get_ptp_edits(hcpcs = "33935", current = TRUE)
```

    > # A tibble: 11 × 7
    >    hcpcs ptp_type     ptp_complements ptp_deleted ptp_edit_mod ptp_edit_mod_desc
    >    <chr> <chr>        <list>          <date>             <int> <chr>            
    >  1 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    >  2 33935 comprehensi… <tibble>        9999-12-31             1 Allowed          
    >  3 33935 comprehensi… <tibble>        9999-12-31             1 Allowed          
    >  4 33935 comprehensi… <tibble>        9999-12-31             1 Allowed          
    >  5 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    >  6 33935 comprehensi… <tibble>        9999-12-31             1 Allowed          
    >  7 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    >  8 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    >  9 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    > 10 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
    > 11 33935 comprehensi… <tibble>        9999-12-31             0 Not Allowed      
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
