# Check whether the data have CWT estimates

This function checks if the raw data include cell wall thickness
estimates, since ROXAS analyses can be run with or without estimating
CWT. We generally require CWT estimates in the case of conifer data, but
not for angiosperms. Thus if any images with all NA cwttan are found,
the function will issue a warning.

## Usage

``` r
check_cwt_estimates(df_rings_log)
```

## Arguments

- df_rings_log:

  the dataframe containing the rings data incl. mean_cwttan

## Value

the input dataframe with an additional logical column 'no_cwt'
