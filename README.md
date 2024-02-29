
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

## Example

``` r
library(northstar)
```

``` r
rvu() |> 
  dplyr::filter(status == "A", 
                work.rvu == max(work.rvu, na.rm = TRUE)) |> 
  dplyr::select(hcpcs, 
                description,
                work.rvu, 
                pe.rvu_nonfac, 
                pe.rvu_fac, 
                mp.rvu, 
                cf) |>
  dplyr::glimpse()
#> Rows: 1
#> Columns: 7
#> $ hcpcs         <chr> "39503"
#> $ description   <chr> "Repair of diaphragm hernia"
#> $ work.rvu      <dbl> 108.91
#> $ pe.rvu_nonfac <dbl> 35.14
#> $ pe.rvu_fac    <dbl> 35.14
#> $ mp.rvu        <dbl> 26.95
#> $ cf            <dbl> 32.7442
```

``` r
gpci() |> 
  dplyr::filter(state == "GA", 
                locality == "01") |>
  dplyr::glimpse()
#> Rows: 1
#> Columns: 7
#> $ mac           <chr> "10212"
#> $ state         <fct> GA
#> $ locality      <chr> "01"
#> $ locality_name <chr> "ATLANTA"
#> $ gpci.pw_floor <dbl> 1
#> $ gpci.pe       <dbl> 0.997
#> $ gpci.mp       <dbl> 1.128
```

``` r
par <- northstar:::calc_payment(
  wk.rvu  = 108.91,
  pe.rvu  = 35.14,
  mp.rvu  = 26.95,
  wk.gpci = 1.0,
  pe.gpci = 0.997,
  mp.gpci = 1.128,
  cf      = 32.7442)

nonpar <- par * 0.95
```

``` r
pp <- function(par    = par, 
               nonpar = nonpar, 
               lc     = limiting_charge(par)) {
  
  glue::glue("Participating Amount:    {gt::vec_fmt_currency(par)}\n",  
             "Non-Particpating Amount: {gt::vec_fmt_currency(nonpar)}\n", 
             "Limiting Charge:         {gt::vec_fmt_currency(lc)}",
             par = par, 
             nonpar = nonpar, 
             lc = lc)
}

pp(par, 
   nonpar, 
   limiting_charge(par))
#> Participating Amount:    $5,708.76
#> Non-Particpating Amount: $5,423.32
#> Limiting Charge:         $6,236.82
```

``` r
payment() |> 
  dplyr::filter(hcpcs == "39503", 
                carrier_no == "10212",
                locality == "01") |> 
  dplyr::glimpse()
#> Rows: 1
#> Columns: 15
#> $ year            <dbl> 2024
#> $ carrier_no      <chr> "10212"
#> $ locality        <chr> "01"
#> $ hcpcs           <chr> "39503"
#> $ mod             <chr> NA
#> $ fee_nonfac      <dbl> 5708.76
#> $ fee_fac         <dbl> 5708.76
#> $ status          <chr> "0"
#> $ mult_surg       <chr> "A"
#> $ ther_red_nonfac <dbl> 2
#> $ flat_visit      <dbl> 0
#> $ ther_red_fac    <dbl> 0
#> $ opps            <chr> "9"
#> $ opps_nonfac     <dbl> 0
#> $ opps_fac        <dbl> 0
```
