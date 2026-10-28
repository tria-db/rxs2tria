# Combine two QWAdata objects

Combines the cells and rings data of two
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
objects with distinct `image_label` values into one. Columns present in
only one object are filled with `NA`; `NA` in logical ring columns is
set to `FALSE`.

## Usage

``` r
combine_QWAdata(x, y)
```

## Arguments

- x, y:

  [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  objects

## Value

A combined
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object

## Details

If the ring flag columns are present (both objects must have them or
neither), the duplicate-ring flags (`duplicate_ring`, `exclude_dupl`)
are recalculated for woodpiece-years with rings from both objects,
replacing any manual duplicate selection there. All other flags are
kept.

## See also

[`combine_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAimages.md),
[`combine_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAmetadata.md)
