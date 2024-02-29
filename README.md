
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
         work.rvu == max(work.rvu, na.rm = TRUE)) |> 
  select(hcpcs, 
         description,
         work.rvu, 
         pe.rvu_nonfac, 
         pe.rvu_fac, 
         mp.rvu, 
         cf) |>
  glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ hcpcs         <chr> "39503"
    > $ description   <chr> "Repair of diaphragm hernia"
    > $ work.rvu      <dbl> 108.91
    > $ pe.rvu_nonfac <dbl> 35.14
    > $ pe.rvu_fac    <dbl> 35.14
    > $ mp.rvu        <dbl> 26.95
    > $ cf            <dbl> 32.7442

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
    > Non-Particpating Amount: $6,236.82
    > Limiting Charge:         $6,236.82

``` r
payment() |> 
  filter(hcpcs      == "39503", 
         carrier_no == "10212",
         locality   == "01") |> 
  select(hcpcs,
         mac = carrier_no,
         locality,
         fee_nonfac,
         fee_fac) |> 
  glimpse()
```

    > Rows: 1
    > Columns: 5
    > $ hcpcs      <chr> "39503"
    > $ mac        <chr> "10212"
    > $ locality   <chr> "01"
    > $ fee_nonfac <dbl> 5708.76
    > $ fee_fac    <dbl> 5708.76
