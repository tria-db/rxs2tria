# Validate a QWAdata object

Re-runs data quality checks on a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object and warns about any issues found. Checks include:

- CWT estimates present for all images.

- Dating validity (undated, duplicate, future years; and if `meta` is
  provided, years after `outmost_year`).

- Flag columns (`incomplete_ring`, `missing_ring`, `duplicate_ring`,
  `exclude_issues`) are present, non-`NA`, and internally consistent
  (e.g. `exclude_issues` agrees with the flag columns given
  `exclude_mode`).

## Usage

``` r
check_QWAdata(x, meta = NULL, exclude_mode = c("either", "incomplete_only"))
```

## Arguments

- x:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

- meta:

  Optional
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object. If provided, also checks that no ring years exceed
  `outmost_year`.

- exclude_mode:

  Which flag combination is expected in `exclude_issues`: `"either"`
  (default) or `"incomplete_only"`. Used to check consistency.

## Value

`TRUE`, invisibly. Warnings are issued for any problems found; errors
are thrown for structural issues.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
