# Write a QWAmetadata object to a JSON file

Empty values are written as `null`; absent (`NULL`) components are not
written.

## Usage

``` r
write_QWAmetadata(
  x,
  file,
  compress = FALSE,
  overwrite = TRUE,
  drop_empty = TRUE
)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object.

- file:

  Path to the output file.

- compress:

  If `TRUE`, write compressed `.gz` file. (default `FALSE`).

- overwrite:

  Allow to overwrite existing files? (default `TRUE`).

- drop_empty:

  If `TRUE` (default), optional fields filled in the Shiny apps
  (`colType` `"meta_input"` or `"qc_input"`) that are empty in all rows
  of a component are not written. Fields required under a condition
  (e.g. `embargoed_until` for restricted datasets) are kept where it
  applies.

## Value

The output file path, invisibly.

## See also

[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
