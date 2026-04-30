# Read a QWAimages object from a CSV file

Reads image-level metadata from a (compressed) CSV file written by
[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md)
and returns a
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object. The `roxas_version` is inferred from the `software` column of
the data.

## Usage

``` r
read_QWAimages(file)
```

## Arguments

- file:

  Path to a `.csv` or `.csv.gz` file.

## Value

A [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object.

## See also

[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md)
