# Check that the data are properly dated

This function checks that the data are properly dated, i.e. that the
year variable is never NA, and does not contain values in the future. If
any invalid years are detected, the function will issue an error.

## Usage

``` r
check_dating(df_rings_log)
```

## Arguments

- df_rings_log:

  the dataframe containing the rings data with cell counts

## Value

the input dataframe with an additional logical column 'undated'
