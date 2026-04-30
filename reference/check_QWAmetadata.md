# Validate a QWAmetadata object

Runs a series of checks on each component of a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object and warns about any issues found. For `$images`, delegates to
[`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md).
For all other components, checks compliance with the corresponding JSON
schema and warns about missing optional columns. Absent components are
skipped.

## Usage

``` r
check_QWAmetadata(x)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object.

## Value

`TRUE`, invisibly. Warnings are issued for any problems found; an error
is thrown if a component fails structural validation.

## See also

[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md),
[`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md)
