
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="175" />

> Tidy Healthcare Revenue Integrity Tools

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/andrewallenbruce/northstar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/andrewallenbruce/northstar?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/andrewallenbruce/northstar/badge)](https://www.codefactor.io/repository/github/andrewallenbruce/northstar)
<br> [![Code
size](https://img.shields.io/github/languages/code-size/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar)
[![Last
commit](https://img.shields.io/github/last-commit/andrewallenbruce/northstar.svg)](https://github.com/andrewallenbruce/northstar/commits/master)
<br>
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

### HCPCS

``` r
# hcpcs_search(hcpcs    = "33935", 
#              state    = "GA", 
#              locality = "01", 
#              mac      = "10212") |> 
#   glimpse()
```

### ICD-10-CM

``` r
icd10cm(code = "T38.0X1A") |> glimpse()
```

    > Rows: 1
    > Columns: 8
    > $ ch          <int> 19
    > $ abbrev      <chr> "INJ"
    > $ chapter     <chr> "Injury, poisoning and certain other consequences of exter…
    > $ range       <chr> "S00 - T88"
    > $ order       <int> 75443
    > $ valid       <int> 1
    > $ code        <chr> "T38.0X1A"
    > $ description <chr> "Poisoning by glucocorticoids and synthetic analogues, acc…

``` r
icd_sections(code = "T38.0X1A") |> glimpse()
```

    > Rows: 1
    > Columns: 4
    > $ code        <chr> "T38.0X1A"
    > $ section     <chr> "T38"
    > $ description <chr> "Poisoning by, adverse effect of and underdosing of hormon…
    > $ n           <int> 336

### Physician Fee Schedule

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

## :classical_building: Governance

This project is primarily maintained by [Andrew
Bruce](https://github.com/andrewallenbruce). Other authors may
occasionally assist with some of these duties.
