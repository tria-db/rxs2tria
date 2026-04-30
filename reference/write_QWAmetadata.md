# Write a QWAmetadata object to a JSON file

Write a QWAmetadata object to a JSON file

## Usage

``` r
write_QWAmetadata(x, file, compress = FALSE, overwrite = TRUE)
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

## Value

The output file path, invisibly.

## See also

[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
