
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

## Return Information about HCPCS Code

``` r
hcpcs_search(hcpcs = "11646", 
             state = "GA", 
             locality = "99", 
             mac = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 51
    > $ hcpcs            <chr> "11646"
    > $ hcpcs_type       <chr> "Category I"
    > $ description      <chr> "Exc f/e/e/n/l mal+mrg >4 cm"
    > $ cons_desc        <chr> "Removal of cancer skin growth of face, ears, eyelids…
    > $ clin_descs       <list> [<tbl_df[10 x 1]>]
    > $ mod              <chr> NA
    > $ status           <chr> "A"
    > $ mac              <chr> "10212"
    > $ state            <chr> "GA"
    > $ locality         <chr> "99"
    > $ area             <chr> "REST OF GEORGIA"
    > $ counties         <chr> "ALL OTHER COUNTIES"
    > $ two_macs         <lgl> FALSE
    > $ wgpci            <dbl> 1
    > $ pgpci            <dbl> 0.883
    > $ mgpci            <dbl> 1.125
    > $ wrvu             <dbl> 6.26
    > $ nonfac_prvu      <dbl> 7.92
    > $ fac_prvu         <dbl> 4.36
    > $ mrvu             <dbl> 0.99
    > $ cf               <dbl> 32.744
    > $ fac_par          <dbl> 367.5065
    > $ nonfac_par       <dbl> 470.4366
    > $ fac_nonpar       <dbl> 349.1312
    > $ nonfac_nonpar    <dbl> 446.9148
    > $ fac_limit        <dbl> 401.5009
    > $ nonfac_limit     <dbl> 513.952
    > $ opps             <chr> "9"
    > $ nonfac_prvu_opps <dbl> 0
    > $ fac_prvu_opps    <dbl> 0
    > $ mrvu_opps        <dbl> 0
    > $ mult_surg        <chr> "0"
    > $ flat_vis         <dbl> 0
    > $ nonfac_therapy   <dbl> 2
    > $ fac_therapy      <dbl> 0
    > $ global           <chr> "010"
    > $ op_ind           <dbl> 1
    > $ op_pre           <dbl> 0.1
    > $ op_intra         <dbl> 0.8
    > $ op_post          <dbl> 0.1
    > $ pctc             <chr> "0"
    > $ mult_proc        <chr> "2"
    > $ surg_bilat       <chr> "0"
    > $ surg_asst        <chr> "1"
    > $ surg_co          <chr> "0"
    > $ surg_team        <chr> "0"
    > $ supvis           <chr> "09"
    > $ dximg            <chr> "99"
    > $ endo             <chr> NA
    > $ rare             <chr> "00"
    > $ unused           <int> 0

``` r
rvu(hcpcs = "V5299") |> glimpse()
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
     locality = "99",
     mac      = "10212") |> 
  glimpse()
```

    > Rows: 1
    > Columns: 9
    > $ mac      <chr> "10212"
    > $ state    <chr> "GA"
    > $ locality <chr> "99"
    > $ name     <chr> "REST OF GEORGIA"
    > $ wgpci    <dbl> 1
    > $ pgpci    <dbl> 0.883
    > $ mgpci    <dbl> 1.125
    > $ counties <chr> "ALL OTHER COUNTIES"
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
