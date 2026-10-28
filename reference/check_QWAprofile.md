# Check a QWAprofile object

Checks a
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object before analysis (profiles are derived data and not imported into
TRIA). Aborts if the object is not a valid `QWAprofile` (key columns
`image_label`, `year` and `sector_n` for sector profiles or
`start`/`end` for band profiles missing, mistyped, `NA` or not unique;
bands with `start >= end`). Warns (or, with `strict = TRUE`, aborts) on:

- no measurement column, or all measurement values `NA` (the count
  columns `<param>_N` and `mrw`, `eww`, `ew_band` are not measurements);

- band profiles with bands of different widths;

- sector profiles with `sector_n` \< 1.

## Usage

``` r
check_QWAprofile(x, strict = FALSE)
```

## Arguments

- x:

  A
  [QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  object.

- strict:

  If `TRUE`, abort on the first issue instead of warning.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## See also

[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md),
[`check_consistency()`](https://tria-db.github.io/rxs2tria/reference/check_consistency.md)
