# Write a QWAimages object to a CSV file.

Write a QWAimages object to a CSV file.

## Usage

``` r
write_QWAimages(x, file, compress = FALSE, overwrite = TRUE, drop_empty = TRUE)
```

## Arguments

- x:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object.

- file:

  Path to the output file.

- compress:

  If `TRUE`, write compressed `.gz` file (default `FALSE`). Note that if
  you add an compression-related extension to the file name, vroom will
  automatically compress the output regardless of this parameter.

- overwrite:

  Allow to overwrite existing files? (default `TRUE`).

- drop_empty:

  If `TRUE` (default), optional columns set in the Shiny apps (`colType`
  `"meta_input"` or `"qc_input"`, e.g. `img_comment`) that are empty in
  all rows are not written.

## Value

The output file path, invisibly.

## Note

Empty values are written as empty fields. The `roxas_version` attribute
is not written to the CSV file. It is re-derived from the `$software`
column when the file is read back with
[`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md).

## See also

[`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md),
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
