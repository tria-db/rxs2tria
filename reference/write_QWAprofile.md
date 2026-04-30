# Write a QWAprofile object to a CSV file

Write a QWAprofile object to a CSV file

## Usage

``` r
write_QWAprofile(x, file, compress = FALSE, overwrite = TRUE)
```

## Arguments

- x:

  A
  [QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  object.

- file:

  Path to the output `.csv` or `.csv.gz` file.

- compress:

  If `TRUE`, write a compressed `.csv.gz` file (default `FALSE`).

- overwrite:

  Allow overwriting existing files? (default `TRUE`).

## Value

The output file path, invisibly.

## Note

The `profile_type` attribute is not written to the CSV file. It is
re-derived from the column names when the file is read back with
[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md).

## See also

[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md),
[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
