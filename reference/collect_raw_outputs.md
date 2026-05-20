# Collect raw output data from multiple files

Read and combine data from multiple cells/rings data files (i.e. the
ROXAS output .txt or ROXAS AI table .csv files with the raw
measurements). Used by
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
to read each `QWAdata` component.

## Usage

``` r
collect_raw_outputs(df_structure, roxas_version, ftype)
```

## Arguments

- df_structure:

  Data frame of image identifiers (`$image_label`) and corresponding
  data file names (`$fname_cells` or `$fname_rings`).

- roxas_version:

  The software used to create the files (`"roxas"` or `"roxas_ai"`).

- ftype:

  which type of files should be read (`"cells"` or `"rings"`).

## Value

A data frame with the relevant raw measurements data from all files
combined.

## See also

[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
