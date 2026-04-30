# Write a QWAdata object to files

Cells and rings are written as (compressed) CSV files.

## Usage

``` r
write_QWAdata(
  x,
  dir = NULL,
  file_cells = NULL,
  file_rings = NULL,
  dataset_name = NULL,
  compress = TRUE,
  overwrite = FALSE
)
```

## Arguments

- x:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

- dir:

  Directory to write to. Files are auto-named using `dataset_name`.
  Mutually exclusive with `file_cells`/`file_rings`.

- file_cells, file_rings:

  Explicit output paths for the cells and rings CSV files. Both must be
  provided together. Mutually exclusive with `dir`.

- dataset_name:

  Name prefix for auto-generated filenames when using `dir`. Defaults to
  `"QWAdata"`.

- compress:

  If `TRUE` (default), write `.csv.gz` files.

- overwrite:

  Allow to overwrite existing files? (default `FALSE`).

## Value

A named list of written file paths, invisibly.

## See also

[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md),
[`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
