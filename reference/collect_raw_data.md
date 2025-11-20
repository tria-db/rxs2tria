# Collect raw cells and rings output data

Read and combine data from all cells and ring output files

## Usage

``` r
collect_raw_data(df_structure)
```

## Arguments

- df_structure:

  Dataframe containing filenames and data structure.

## Value

`QWA_data`, i.e. a named list containing the combined raw data for cells
and rings in two dataframes under `$cells` and `$rings`, respectively.
