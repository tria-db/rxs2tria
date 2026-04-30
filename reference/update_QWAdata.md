# Update a QWAdata object with re-read raw data for selected images

Re-reads the raw data files for `imgs_to_update`, recomputes any derived
cell/ring measures and derived flag columns that are already present in
`x`, and splices the result back into the existing
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object.

## Usage

``` r
update_QWAdata(
  x,
  imgs_to_update,
  meta,
  exclude_mode = c("either", "incomplete_only"),
  reset_manual_flags = TRUE
)
```

## Arguments

- x:

  a [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object

- imgs_to_update:

  character vector of `image_label` values to update; must be a subset
  of `meta$image_label`

- meta:

  a
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object, required for
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)
  when flag columns are present in `x`

- exclude_mode:

  passed to
  [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md);
  only relevant when the `$exclude_issues` flag is present in `x`: how
  should the `$exclude_issues` flag column be initialized, to exclude
  any incomplete or missing rings (`"either"`, default) or only the
  incomplete rings (i.e., not flagging missing/wedging rings for
  exclusion from analyses, `"incomplete_only"`).

- reset_manual_flags:

  Logical; if `TRUE` (default), any manual flag column edits made via
  the flags Shiny app for the updated images are reset. If `FALSE`,
  existing values in the manual flag columns are kept (i.e. this assumes
  that the dating was not affected by the re-analysis).

## Value

An updated
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object

## Details

Duplicate-ring flags are recalculated across all images sharing a
woodpiece with any updated image, because overlaps may change when one
image changes.
