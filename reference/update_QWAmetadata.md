# Update a QWAmetadata object to match a QWAimages object

Replaces the `$images` component of `x` with `images` and updates the
hierarchical tables (`$sites`, `$trees`, `$woodpieces`, `$slides`)
accordingly:

- the hierarchy columns and counts (`n_trees`, `n_woodpieces`,
  `n_slides`, `n_images`) are derived from `images`; `species_code` is
  kept from `x` where it is `NA` in `images`,

- all other columns are kept from `x`,

- rows for IDs new in `images` are added (other columns `NA`),

- rows for IDs no longer in `images` are removed.

## Usage

``` r
update_QWAmetadata(x, images)
```

## Arguments

- x:

  a
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object

- images:

  a
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object; the complete set of images of the updated dataset

## Value

An updated
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object

## Details

Tables that are `NULL` in `x` remain `NULL` (see
[`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md)).
The image columns set in the apps (`colType` `"meta_input"`, e.g.
`only_ew`, `img_comment` from the metadata app, and `"qc_input"`, e.g.
`comment_handled` from the flags app) are filled from `x$images` where
they are missing or `NA` in `images`, e.g. after rebuilding it with
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md).

Submission files only; TRIA dataset files are not edited.

## See also

[`update_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/update_QWAimages.md),
[`update_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAdata.md),
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md)
