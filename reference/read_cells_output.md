# Read single cell output file

Helper function to read a single (classical) ROXAS cells output file

## Usage

``` r
read_cells_output(file_cells, selcols_cells, colname_variants)
```

## Arguments

- file_cells:

  filename to be read

- selcols_cells:

  character vector of the required columns to keep

- colname_variants:

  named character vector of any variant column names to rename

## Value

A dataframe with the raw data (relevant columns only).
