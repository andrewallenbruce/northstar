
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar

<!-- badges: start -->
<!-- badges: end -->

The goal of **northstar** is to …

## :package: Installation

You can install **northstar** from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("andrewallenbruce/northstar")
```

## :beginner: Usage

**Fee Schedule Calculation**

``` r
library(northstar)
library(dplyr)
```

``` r
rvu <- rvu(hcpcs = "11646") |> 
  select(hcpcs, 
         description,
         wrvu, 
         prvu_nf, 
         prvu_f, 
         mrvu, 
         cf)

rvu |> glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ hcpcs       <chr> "11646"
    > $ description <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ wrvu        <dbl> 6.26
    > $ prvu_nf     <dbl> 7.92
    > $ prvu_f      <dbl> 4.36
    > $ mrvu        <dbl> 0.99
    > $ cf          <dbl> 32.7442

``` r
gp <- gpci(
  state    = "GA",
  locality = "99",
  mac      = "10212")

gp |> glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ mac      <chr> "10212"
    > $ state    <fct> GA
    > $ locality <chr> "99"
    > $ name     <chr> "REST OF GEORGIA"
    > $ wgpci    <dbl> 1
    > $ pgpci    <dbl> 0.883
    > $ mgpci    <dbl> 1.125

``` r
calc_amounts(wrvu     = rvu$wrvu,
             prvu_nf  = rvu$prvu_nf,
             prvu_f   = rvu$prvu_f,
             mrvu     = rvu$mrvu,
             cf       = rvu$cf,
             wgpci    = gp$wgpci,
             pgpci    = gp$pgpci,
             mgpci    = gp$mgpci)
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
payment(hcpcs    = "11646", 
        mac      = "10212",
        locality = "99") |> 
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
