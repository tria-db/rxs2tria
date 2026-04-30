# Read a QWAprofile object from a CSV file

Reads a profile written by
[`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
and reconstructs the
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object. The `profile_type` is inferred from the available columns.

## Usage

``` r
read_QWAprofile(file)
```

## Arguments

- file:

  Path to a `.csv` or `.csv.gz` file.

## Value

A
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object.

## See also

[`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
