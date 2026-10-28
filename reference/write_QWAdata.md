# Write a QWAdata object to files

Cells and rings are written as (compressed) CSV files. Empty values are
written as empty fields.

## Usage

``` r
write_QWAdata(
  x,
  dir = NULL,
  file_cells = NULL,
  file_rings = NULL,
  dataset_name = NULL,
  compress = TRUE,
  overwrite = FALSE,
  drop_empty = TRUE,
  keep_derived = TRUE
)
```

## Arguments

- x:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

- dir:

  Directory to write to. Files are auto-named
  `{dataset_name}_QWAcells.csv(.gz)` and
  `{dataset_name}_QWArings.csv(.gz)`. Mutually exclusive with
  `file_cells`/`file_rings`.

- file_cells, file_rings:

  Explicit output paths for the cells and rings CSV files. Both must be
  provided together. Mutually exclusive with `dir`.

- dataset_name:

  Name prefix for auto-generated filenames when using `dir`. If omitted,
  files are named `QWAcells.csv(.gz)` and `QWArings.csv(.gz)`.

- compress:

  If `TRUE` (default), write `.csv.gz` files.

- overwrite:

  Allow to overwrite existing files? (default `FALSE`).

- drop_empty:

  If `TRUE` (default), optional columns set in the flags app (`colType`
  `"qc_input"`, e.g. ring quality flags and comments) that are empty
  (`NA`, or `NA`/`FALSE` for flags) in all rows are not written.

- keep_derived:

  If `FALSE`, derived columns (`colType` `"derived"`, e.g. cell measures
  computed from other columns) are not written. They can be recomputed
  with
  [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md).
  Default `TRUE`.

## Value

A named list of written file paths, invisibly.

## See also

[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md),
[`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
