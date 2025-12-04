# Read single rings output file

Helper function to read a single (classical) ROXAS rings output file

## Usage

``` r
read_rings_output(file_rings, selcols_rings, colname_variants)
```

## Arguments

- file_rings:

  filename to be read

- colname_variants:

  named character vector of any variant column names to rename

- selcols_cells:

  character vector of the required columns to keep

## Value

A dataframe with the raw data (relevant columns only).
