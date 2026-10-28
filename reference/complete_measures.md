# Complete QWA measures

Some additional cell measures are calculated based on the existing data:

- tca: la + cwa

- rwd2: cwtrad/drad

- dcwt

- cwtall_adj

- cdrad, cdtan, cdratio

- sector100

- ew_lw: indicates if it is an earlywood (`"ew"`) or latewood (`"lw"`)
  cell (based on Mork index \<1 for EW) And for the rings, we add

- eww and lww estimates (based on Mork index of 1) If any of these
  columns already exist in the data they are NOT recalculated, only the
  missing measures are added.

## Usage

``` r
complete_measures(QWA_data)
```

## Arguments

- QWA_data:

  a `QWAdata` object containing the cells and rings data frames

## Value

a `QWAdata` object with the updated cells and rings dataframes with the
new measures
