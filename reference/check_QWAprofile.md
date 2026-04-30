# Validate a QWAprofile object

Runs a series of checks on a
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object and warns about any issues found. Checks include: required
identifier columns present and non-`NA`, consistent bin definitions
(`sector_n` for sector profiles, `start`/`end` for band profiles), and
at least one measurement column present.

## Usage

``` r
check_QWAprofile(x)
```

## Arguments

- x:

  A
  [QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  object.

## Value

`TRUE`, invisibly. Warnings are issued for any problems found.

## See also

[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
