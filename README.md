
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
  filter(hcpcs == "11646") |> 
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
         locality == "99") |>
  select(-ftnote) |> 
  glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ mac           <chr> "10212"
    > $ state         <fct> GA
    > $ locality      <chr> "99"
    > $ locality_name <chr> "REST OF GEORGIA"
    > $ wgpci         <dbl> 1
    > $ pgpci         <dbl> 0.883
    > $ mgpci         <dbl> 1.125

``` r
calc_amounts(wrvu     = 6.26,
             prvu_nf  = 7.92,
             prvu_f   = 4.36,
             mrvu     = 0.99,
             wgpci    = 1.0,
             pgpci    = 0.883,
             mgpci    = 1.125,
             cf       = 32.7442)
```

    > Facility:
    > Participating Amount    = $367.51
    > Non-Particpating Amount = $349.13
    > Limiting Charge         = $401.50
    > 
    > Non-Facility:
    > Participating Amount    = $470.44
    > Non-Particpating Amount = $446.92
    > Limiting Charge         = $513.96

``` r
payment() |> 
  filter(hcpcs    == "11646", 
         mac      == "10212",
         locality == "99") |> 
  select(hcpcs,
         mac,
         locality,
         fee_nf,
         fee_f) |> 
  glimpse()
```

    > Rows: 1
    > Columns: 5
    > $ hcpcs    <chr> "11646"
    > $ mac      <chr> "10212"
    > $ locality <chr> "99"
    > $ fee_nf   <dbl> 470.44
    > $ fee_f    <dbl> 367.51
