# Complete a QWAimages object

Add any missing columns to a
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object, including optional columns, except the quality control columns
set in the flags app (`colType` `"qc_input"`, e.g. `comment_handled`).
New columns are initialised as `NA`.

## Usage

``` r
complete_QWAimages(x)
```

## Arguments

- x:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object.

## Value

A [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object extended with all optional columns except `qc_input` ones.

## See also

[`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md),
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
