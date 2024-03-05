
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
rvu(hcpcs = "11646") |> 
  select(hcpcs, 
         description,
         wrvu, 
         nprvu, 
         fprvu, 
         mrvu, 
         cf) |> 
  glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ hcpcs       <chr> "11646"
    > $ description <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ wrvu        <dbl> 6.26
    > $ nprvu       <dbl> 7.92
    > $ fprvu       <dbl> 4.36
    > $ mrvu        <dbl> 0.99
    > $ cf          <dbl> 32.744

``` r
gpci(
  state    = "GA",
  locality = "99",
  mac      = "10212") |> 
  glimpse()
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
calc_amounts(wrvu  = 6.26,
             nprvu = 7.92,
             fprvu = 4.36,
             mrvu  = 0.99,
             cf    = 32.744,
             wgpci = 1,
             pgpci = 0.883,
             mgpci = 1.125)
```

    > Facility:
    > Participating Amount    = $367.51
    > Non-Particpating Amount = $349.13
    > Limiting Charge         = $401.50
    > 
    > Non-Facility:
    > Participating Amount    = $470.44
    > Non-Particpating Amount = $446.91
    > Limiting Charge         = $513.95

``` r
pfs(hcpcs    = "11646", 
    mac      = "10212",
    locality = "99") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 10
    > $ mac       <chr> "10212"
    > $ locality  <chr> "99"
    > $ hcpcs     <chr> "11646"
    > $ mod       <chr> "00"
    > $ status    <chr> "A"
    > $ mult_surg <chr> "0"
    > $ flat_vis  <dbl> 0
    > $ ntherapy  <dbl> 2
    > $ ftherapy  <dbl> 0
    > $ opps      <chr> "0"

## Return Information about HCPCS Code

``` r
calc_amounts_df(hcpcs    = "11646", 
                state    = "GA", 
                locality = "99", 
                mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 46
    > $ hcpcs       <chr> "11646"
    > $ description <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ mod         <chr> "00"
    > $ status      <chr> "A"
    > $ wrvu        <dbl> 6.26
    > $ nprvu       <dbl> 7.92
    > $ fprvu       <dbl> 4.36
    > $ mrvu        <dbl> 0.99
    > $ cf          <dbl> 32.744
    > $ nprvu_opps  <dbl> 0
    > $ fprvu_opps  <dbl> 0
    > $ mrvu_opps   <dbl> 0
    > $ global      <chr> "010"
    > $ op_ind      <dbl> 1
    > $ op_pre      <dbl> 0.1
    > $ op_intra    <dbl> 0.8
    > $ op_post     <dbl> 0.1
    > $ pctc        <chr> "0"
    > $ mult_proc   <int> 2
    > $ surg_bilat  <int> 0
    > $ surg_asst   <int> 1
    > $ surg_co     <int> 0
    > $ surg_team   <int> 0
    > $ endo        <chr> NA
    > $ supvis      <chr> "09"
    > $ dximg       <dbl> 0
    > $ unused      <int> 0
    > $ rare        <chr> "00"
    > $ mac         <chr> "10212"
    > $ state       <fct> GA
    > $ locality    <chr> "99"
    > $ name        <chr> "REST OF GEORGIA"
    > $ wgpci       <dbl> 1
    > $ pgpci       <dbl> 0.883
    > $ mgpci       <dbl> 1.125
    > $ mult_surg   <chr> "0"
    > $ flat_vis    <dbl> 0
    > $ ntherapy    <dbl> 2
    > $ ftherapy    <dbl> 0
    > $ opps        <chr> "0"
    > $ fpar        <dbl> 367.5065
    > $ npar        <dbl> 470.4366
    > $ fnpar       <dbl> 349.1312
    > $ nnpar       <dbl> 446.9148
    > $ flim        <dbl> 401.5009
    > $ nlim        <dbl> 513.952
