# Collect raw output data from multiple files

Read and combine data from multple cells/rings data files (i.e. the
ROXAS output txts or ROXAS AI table csvs with the raw measurements).
Used by
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
to read each `QWAdata` component.

## Usage

``` r
collect_raw_outputs(df_structure, roxas_version, ftype)
```

## Arguments

- df_structure:

  Data frame containing filenames (`$fname_cells` or `$fname_rings`) and
  image labels (`$image_label`).

- roxas_version:

  The software used to create the files (`"roxas"` or `"roxas_ai"`).

- ftype:

  which type of files should be read (`"cells"` or `"rings"`).

## Value

A data frame with the relevant raw measurements data from all files
combined.

## See also

[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
