# Complete a QWAdata object

Runs the full preprocessing pipeline on a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object:

1.  Adds derived cell measures (see
    [`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)).

2.  Extends the rings data frame to cover all years present in cells,
    with cell counts and mean CWT per ring (see `complete_rings_log()`).

3.  Flags incomplete, missing, and duplicate rings and initialises the
    `exclude_issues` column (see
    [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)).

## Usage

``` r
complete_QWAdata(x, meta, exclude_mode = c("either", "incomplete_only"))
```

## Arguments

- x:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object with both `$cells` and `$rings`.

- meta:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object providing `spatial_resolution` and `outmost_year`, required for
  the incomplete-innermost and dating checks.

- exclude_mode:

  Passed to
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md):
  `"either"` (default) excludes incomplete and missing rings;
  `"incomplete_only"` excludes only incomplete rings.

## Value

A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object with all derived columns and flag columns populated.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md),
[`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md),
[`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)
