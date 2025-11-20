# Complete the rings df with all years present in the cell data

This function takes the QWA data (cells and rings dataframes) and
returns an extended version of the rings dataframe, with additional rows
for years that were only present in the cells data, and additional
columns for the number of cells per ring (cno) and the mean cwttan per
ring (mean_cwttan; this is later used to check if CWT estimates were
made).

## Usage

``` r
complete_rings_log(QWA_data)
```

## Arguments

- QWA_data:

  a list containing the cells and rings dataframes

## Value

an extended dataframe of rings data
