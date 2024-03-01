
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar

<!-- badges: start -->
<!-- badges: end -->

The goal of **northstar** is to …

## Installation

You can install the development version of **northstar** from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("andrewallenbruce/northstar")
```

## Example: Payment Amount Calculation

``` r
library(northstar)
library(dplyr)
```

``` r
rvu() |> 
  filter(status   == "A",
         wrvu == max(wrvu, na.rm = TRUE)) |> 
  select(hcpcs, 
         description,
         wrvu, 
         prvu_nf, 
         prvu_f, 
         mrvu, 
         cf) |>
  glimpse()
```

``` r
gpci() |> 
  filter(state    == "GA", 
         locality == "01") |>
  glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ mac           <chr> "10212"
    > $ state         <fct> GA
    > $ locality      <chr> "01"
    > $ locality_name <chr> "ATLANTA"
    > $ gpci.pw_floor <dbl> 1
    > $ gpci.pe       <dbl> 0.997
    > $ gpci.mp       <dbl> 1.128

``` r
calc_amounts(wrvu  = 108.91,
             prvu  = 35.14,
             mrvu  = 26.95,
             wgpci = 1.0,
             pgpci = 0.997,
             mgpci = 1.128,
             cf    = 32.7442)
```

    > Participating Amount:    $5,708.76
    > Non-Particpating Amount: $5,423.32
    > Limiting Charge:         $6,236.82

``` r
payment() |> 
  filter(hcpcs      == "39503", 
         carrier_no == "10212",
         locality   == "01") |> 
  select(hcpcs,
         mac,
         locality,
         fee_nf,
         fee_f) |> 
  glimpse()
```
