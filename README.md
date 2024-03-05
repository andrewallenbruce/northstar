
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
         nf_prvu, 
         f_prvu, 
         mrvu, 
         cf)

rvu |> glimpse()
```

    > Rows: 1
    > Columns: 7
    > $ hcpcs       <chr> "11646"
    > $ description <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ wrvu        <dbl> 6.26
    > $ nf_prvu     <dbl> 7.92
    > $ f_prvu      <dbl> 4.36
    > $ mrvu        <dbl> 0.99
    > $ cf          <dbl> 32.744

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
             prvu_nf  = rvu$nf_prvu,
             prvu_f   = rvu$f_prvu,
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
    > Non-Particpating Amount = $446.91
    > Limiting Charge         = $513.95

``` r
pfs(hcpcs    = "11646", 
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

## Return Information about HCPCS Code

``` r
calc_amounts_df(hcpcs    = "11646", 
                state    = "GA", 
                locality = "99", 
                mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 55
    > $ hcpcs         <chr> "11646"
    > $ mod.x         <chr> "00"
    > $ description   <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ status.x      <chr> "A"
    > $ unused        <int> 0
    > $ wrvu          <dbl> 6.26
    > $ nf_prvu       <dbl> 7.92
    > $ nf_rare       <int> 0
    > $ f_prvu        <dbl> 4.36
    > $ f_rare        <int> 0
    > $ mrvu          <dbl> 0.99
    > $ nf_total      <dbl> 15.17
    > $ f_total       <dbl> 11.61
    > $ pctc          <chr> "0"
    > $ global        <chr> "010"
    > $ op_pre        <dbl> 0.1
    > $ op_intra      <dbl> 0.8
    > $ op_post       <dbl> 0.1
    > $ mult_proc     <int> 2
    > $ surg_bilat    <int> 0
    > $ surg_asst     <int> 1
    > $ surg_co       <int> 0
    > $ surg_team     <int> 0
    > $ endo          <chr> NA
    > $ cf            <dbl> 32.744
    > $ supvis        <chr> "09"
    > $ dximg         <dbl> 0
    > $ nf_prvu_opps  <dbl> 0
    > $ f_prvu_opps   <dbl> 0
    > $ mrvu_opps     <dbl> 0
    > $ mac           <chr> "10212"
    > $ state         <fct> GA
    > $ locality      <chr> "99"
    > $ name          <chr> "REST OF GEORGIA"
    > $ wgpci         <dbl> 1
    > $ pgpci         <dbl> 0.883
    > $ mgpci         <dbl> 1.125
    > $ year          <dbl> 2024
    > $ mod.y         <chr> NA
    > $ fee_nf        <dbl> 470.44
    > $ fee_f         <dbl> 367.51
    > $ status.y      <chr> "0"
    > $ mult_surg     <chr> "A"
    > $ therapy_nf    <dbl> 2
    > $ flatfee_visit <dbl> 0
    > $ therapy_f     <dbl> 0
    > $ opps          <chr> "9"
    > $ opps_nf       <dbl> 0
    > $ opps_f        <dbl> 0
    > $ par_amt_f     <dbl> 367.5065
    > $ par_amt_nf    <dbl> 470.4366
    > $ nonpar_amt_f  <dbl> 349.1312
    > $ nonpar_amt_nf <dbl> 446.9148
    > $ limit_f       <dbl> 401.5009
    > $ limit_nf      <dbl> 513.952
