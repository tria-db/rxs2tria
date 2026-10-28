# Complete a QWAdata object

Runs the full preprocessing pipeline on a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object:

1.  Aligns column types and extends `$rings` to complete year sequences
    per image, with the cell count `cno` from `$cells`; warns on dating
    problems.

2.  Adds derived cell measures and the earlywood/latewood estimates (see
    [`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)).

3.  Flags incomplete, missing, and duplicate rings and initialises
    `exclude_dupl` and `exclude_issues` (see
    [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)).

## Usage

``` r
complete_QWAdata(x, meta, exclude_mode = NULL)
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

  Deprecated, see
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md).

## Value

A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object with all derived columns and flag columns populated.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md),
[`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md),
[`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)
