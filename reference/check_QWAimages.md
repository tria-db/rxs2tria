# Validate a QWAimages object

Runs a series of checks on a
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object and warns about any issues found. Checks include: compliance with
the base JSON schema, consistency of the `roxas_version` attribute with
the `$software` column, missing optional columns, and invalid data
structure.

## Usage

``` r
check_QWAimages(x)
```

## Arguments

- x:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object.

## Value

`TRUE`, invisibly. Warnings are issued for any problems found; an error
is thrown if the object structure is invalid.

## See also

[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md),
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md)
