
<!-- README.md is generated from README.Rmd. Please edit that file -->

# northstar <img src="man/figures/logo.png" align="right" height="150" />

<!-- badges: start -->
<!-- badges: end -->

> Tidy Healthcare Revenue Integrity Tools

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

## Return Information about HCPCS Code

``` r
hcpcs_search(hcpcs    = "33935", 
             state    = "GA", 
             locality = "01", 
             mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 54
    > $ hcpcs                 <chr> "33935"
    > $ level                 <chr> "1"
    > $ category              <chr> "1"
    > $ description           <chr> "Transplantation heart/lung"
    > $ description_consumer  <chr> "Transplantation of donor heart and lung"
    > $ description_clinician <list> [<tbl_df[1 x 1]>]
    > $ section               <chr> "Surgery [10004-69990]"
    > $ rbcs_category         <chr> "Procedure"
    > $ rbcs_subcategory      <chr> "Cardiovascular"
    > $ rbcs_family           <chr> "No RBCS Family"
    > $ rbcs_procedure        <chr> "Major"
    > $ status                <chr> "Bundled/Excluded Code"
    > $ mac                   <chr> "10212"
    > $ state                 <chr> "GA"
    > $ locality              <chr> "01"
    > $ area                  <chr> "ATLANTA"
    > $ wgpci                 <dbl> 1
    > $ pgpci                 <dbl> 0.997
    > $ mgpci                 <dbl> 1.128
    > $ wrvu                  <dbl> 91.78
    > $ nonfac_prvu           <dbl> 31.07
    > $ fac_prvu              <dbl> 31.07
    > $ mrvu                  <dbl> 21.24
    > $ cf                    <dbl> 32.744
    > $ fac_par               <dbl> 4804.053
    > $ nonfac_par            <dbl> 4804.053
    > $ fac_nonpar            <dbl> 4563.85
    > $ nonfac_nonpar         <dbl> 4563.85
    > $ fac_limit             <dbl> 5248.428
    > $ nonfac_limit          <dbl> 5248.428
    > $ opps                  <chr> "9"
    > $ nonfac_prvu_opps      <dbl> 0
    > $ fac_prvu_opps         <dbl> 0
    > $ mrvu_opps             <dbl> 0
    > $ mult_surg             <chr> "0"
    > $ mult_proc             <chr> "2"
    > $ flat_vis              <dbl> 0
    > $ nonfac_ther           <dbl> 2
    > $ fac_ther              <dbl> 0
    > $ global                <chr> "090"
    > $ op_pre                <dbl> 0.09
    > $ op_intra              <dbl> 0.84
    > $ op_post               <dbl> 0.07
    > $ mod                   <chr> NA
    > $ pctc                  <chr> "0"
    > $ surg_bilat            <chr> "0"
    > $ surg_asst             <chr> "2"
    > $ surg_co               <chr> "1"
    > $ surg_team             <chr> "2"
    > $ supvis                <chr> "09"
    > $ dximg                 <chr> "99"
    > $ endo                  <chr> NA
    > $ rare                  <chr> "10"
    > $ unused                <int> 0

``` r
rvu(hcpcs = "V5299") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 28
    > $ hcpcs       <chr> "V5299"
    > $ description <chr> "Hearing service"
    > $ mod         <chr> NA
    > $ status      <chr> "R"
    > $ wrvu        <dbl> 0
    > $ nprvu       <dbl> 0
    > $ fprvu       <dbl> 0
    > $ mrvu        <dbl> 0
    > $ cf          <dbl> 32.744
    > $ nprvu_opps  <dbl> 0
    > $ fprvu_opps  <dbl> 0
    > $ mrvu_opps   <dbl> 0
    > $ global      <chr> "XXX"
    > $ op_ind      <dbl> 0
    > $ op_pre      <dbl> 0
    > $ op_intra    <dbl> 0
    > $ op_post     <dbl> 0
    > $ pctc        <chr> "0"
    > $ mult_proc   <chr> "0"
    > $ surg_bilat  <chr> "0"
    > $ surg_asst   <chr> "0"
    > $ surg_co     <chr> "0"
    > $ surg_team   <chr> "0"
    > $ endo        <chr> NA
    > $ supvis      <chr> "09"
    > $ dximg       <chr> "99"
    > $ unused      <int> 0
    > $ rare        <chr> "00"

``` r
gpci(state    = "GA",
     locality = "01",
     mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 9
    > $ mac      <chr> "10212"
    > $ state    <chr> "GA"
    > $ locality <chr> "01"
    > $ name     <chr> "ATLANTA"
    > $ wgpci    <dbl> 1
    > $ pgpci    <dbl> 0.997
    > $ mgpci    <dbl> 1.128
    > $ counties <chr> "BUTTS, CHEROKEE, CLAYTON, COBB, DEKALB, DOUGLAS, FAYETTE, FO…
    > $ two_macs <lgl> FALSE

``` r
pfs(hcpcs    = "11646", 
    mac      = "10212",
    locality = "99") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 14
    > $ mac       <chr> "10212"
    > $ locality  <chr> "99"
    > $ hcpcs     <chr> "11646"
    > $ mod       <chr> NA
    > $ status    <chr> "A"
    > $ mult_surg <chr> "0"
    > $ flat_vis  <dbl> 0
    > $ nther     <dbl> 2
    > $ fther     <dbl> 0
    > $ fee_nf    <dbl> 470.44
    > $ fee_f     <dbl> 367.51
    > $ opps      <chr> "9"
    > $ opps_nf   <dbl> 0
    > $ opps_f    <dbl> 0

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
