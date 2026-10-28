# Check a QWAdata object

Checks a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object before submission. Aborts if the object is not a valid `QWAdata`
(missing, mistyped or `NA` keys, duplicate (image, year) in `$rings`).
Warns (or, with `strict = TRUE`, aborts) on:

- values violating the cells or rings schema (types, ranges, allowed
  values);

- duplicate cell keys (image, year, `xpix`, `ypix`), the key of the
  cells in TRIA;

- dating problems: gaps and future years;

- missing values in the automatic ring flags (see
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md));

- images only in `$cells` or only in `$rings`, and images without ring
  measurements (no `mrw` \> 0; check the output files and re-read them
  with
  [`update_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAdata.md));

- years with cells but without a rings row, and `cno` not matching the
  number of cells (re-run
  [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)).

## Usage

``` r
check_QWAdata(x, strict = FALSE, exclude_mode = NULL)
```

## Arguments

- x:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

- strict:

  If `TRUE`, abort on the first issue instead of warning.

- exclude_mode:

  Deprecated and ignored.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## Details

Also informs about missing optional columns, extra columns (kept, but
not imported into TRIA by default) and images without CWT estimates.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md),
[`check_consistency()`](https://tria-db.github.io/rxs2tria/reference/check_consistency.md)
