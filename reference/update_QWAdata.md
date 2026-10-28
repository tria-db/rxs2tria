# Update a QWAdata object to match a QWAimages object

Synchronises `x` with the images in `meta`:

- images in `meta` but not in `x` are read and added,

- images in `x` but not in `meta` are removed,

- images in `imgs_to_reread` are read again from their raw data files
  and replace the existing data (e.g. after re-analysis in ROXAS/ROXAS
  AI).

## Usage

``` r
update_QWAdata(
  x,
  meta,
  imgs_to_reread = NULL,
  exclude_mode = NULL,
  reset_manual_flags = TRUE
)
```

## Arguments

- x:

  a [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object

- meta:

  a
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object; defines the images in the updated data

- imgs_to_reread:

  optional character vector of `image_label` values whose raw data files
  are read again; must be a subset of `meta$image_label`. `NULL`
  (default) re-reads none.

- exclude_mode:

  Deprecated, see
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md).

- reset_manual_flags:

  Logical; only applies to `imgs_to_reread`. If `TRUE` (default), any
  manual edits made via the flags Shiny app are reset. If `FALSE` (i.e.
  assuming the dating was not affected by the re-analysis), the columns
  set in the flags app (`colType` `"qc_input"`) are kept, and so is
  `exclude_issues` where it differs from the value derived from the old
  automatic flags.

## Value

An updated
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object

## Details

For all read images, derived cell/ring measures and flag columns that
are already present in `x` are recalculated. Duplicate-ring flags
(`duplicate_ring`, `exclude_dupl`) are recalculated for all
woodpiece-years that contain an added, removed or re-read image, since
overlaps may change; any manual duplicate selection in these
woodpiece-years is replaced.

Added and re-read images are treated as new data: their flags need to be
inspected again, e.g. in the flags Shiny app or by re-applying any
custom rule for `exclude_issues`. Only if a re-analysis was minor and
did not change the dating, use `reset_manual_flags = FALSE` to keep the
previous manual flags.
