
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
describe_hcpcs(hcpcs = c("33924", "43116"))
```

    > # A tibble: 11 × 8
    >    hcpcs description  desc_type level category section rbcs_category rbcs_family
    >    <chr> <chr>        <chr>     <chr> <chr>    <chr>   <chr>         <chr>      
    >  1 33924 Remove pulm… Short     I     I        Surgery Major Proced… Cardiovasc…
    >  2 33924 Ligation an… Long      I     I        Surgery Major Proced… Cardiovasc…
    >  3 33924 LIG&TKDN SY… Medical   I     I        Surgery Major Proced… Cardiovasc…
    >  4 33924 Disconnecti… Consumer  I     I        Surgery Major Proced… Cardiovasc…
    >  5 33924 Ligation an… Clinician I     I        Surgery Major Proced… Cardiovasc…
    >  6 33924 Ligation an… Clinician I     I        Surgery Major Proced… Cardiovasc…
    >  7 43116 Partial rem… Short     I     I        Surgery Major Proced… Digestive/…
    >  8 43116 Partial eso… Long      I     I        Surgery Major Proced… Digestive/…
    >  9 43116 PRTL ESOPHA… Medical   I     I        Surgery Major Proced… Digestive/…
    > 10 43116 Partial rem… Consumer  I     I        Surgery Major Proced… Digestive/…
    > 11 43116 Partial cer… Clinician I     I        Surgery Major Proced… Digestive/…

## NCCI Edits

### Add-On Codes

``` r
get_addon_edits(hcpcs = c("33924", "33935"))
```

    > # A tibble: 6 × 6
    >   hcpcs aoc_type aoc_complements   aoc_edit_type aoc_edit_description           
    >   <chr> <chr>    <list>                    <int> <chr>                          
    > 1 33924 addon    <tibble [39 × 1]>             1 Only Paid if Primary is Paid. …
    > 2 33924 addon    <tibble [10 × 1]>             1 Only Paid if Primary is Paid. …
    > 3 33924 addon    <tibble [1 × 1]>              1 Only Paid if Primary is Paid. …
    > 4 33924 addon    <tibble [3 × 1]>              1 Only Paid if Primary is Paid. …
    > 5 33935 primary  <tibble [1 × 1]>              1 Only Paid if Primary is Paid. …
    > 6 33935 primary  <tibble [3 × 1]>              1 Only Paid if Primary is Paid. …
    > # ℹ 1 more variable: aoc_edit_effective <int>

### Retrieve MUEs

``` r
get_mue_edits(hcpcs = c("33924", "33935"))
```

    > # A tibble: 4 × 6
    >   hcpcs mue_uos mue_mai mue_mai_desc              mue_service_type mue_rationale
    >   <chr>   <int>   <int> <chr>                     <chr>            <chr>        
    > 1 33924       1       2 Date of Service Edit: Po… Practitioner     Code Descrip…
    > 2 33935       1       2 Date of Service Edit: Po… Practitioner     Anatomic Con…
    > 3 33924       1       2 Date of Service Edit: Po… Outpatient Hosp… Code Descrip…
    > 4 33935       1       2 Date of Service Edit: Po… Outpatient Hosp… Anatomic Con…

### Procedure-to-Procedure Edits

``` r
get_ptp_edits(hcpcs = c("33924", "33935"))
```

    > # A tibble: 21 × 6
    >    hcpcs ptp_type      ptp_complements   ptp_edit_mod ptp_edit_mod_desc
    >    <chr> <chr>         <list>                   <int> <chr>            
    >  1 33924 comprehensive <tibble [1 × 1]>             0 Not Allowed      
    >  2 33924 comprehensive <tibble [4 × 1]>             0 Not Allowed      
    >  3 33924 comprehensive <tibble [1 × 1]>             1 Allowed          
    >  4 33924 comprehensive <tibble [4 × 1]>             1 Allowed          
    >  5 33924 comprehensive <tibble [6 × 1]>             0 Not Allowed      
    >  6 33924 comprehensive <tibble [2 × 1]>             1 Allowed          
    >  7 33924 comprehensive <tibble [2 × 1]>             1 Allowed          
    >  8 33924 comprehensive <tibble [2 × 1]>             1 Allowed          
    >  9 33924 comprehensive <tibble [1 × 1]>             0 Not Allowed      
    > 10 33924 component     <tibble [1 × 1]>             0 Not Allowed      
    > 11 33935 comprehensive <tibble [27 × 1]>            0 Not Allowed      
    > 12 33935 comprehensive <tibble [50 × 1]>            1 Allowed          
    > 13 33935 comprehensive <tibble [63 × 1]>            1 Allowed          
    > 14 33935 comprehensive <tibble [67 × 1]>            1 Allowed          
    > 15 33935 comprehensive <tibble [13 × 1]>            0 Not Allowed      
    > 16 33935 comprehensive <tibble [1 × 1]>             1 Allowed          
    > 17 33935 comprehensive <tibble [2 × 1]>             0 Not Allowed      
    > 18 33935 comprehensive <tibble [2 × 1]>             0 Not Allowed      
    > 19 33935 comprehensive <tibble [11 × 1]>            0 Not Allowed      
    > 20 33935 comprehensive <tibble [24 × 1]>            0 Not Allowed      
    > # ℹ 1 more row
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
