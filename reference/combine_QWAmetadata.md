# Combine two QWAmetadata objects

Combines two
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
objects, e.g. prepared from separate input folders, into one. The images
are combined with
[`combine_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAimages.md),
so both objects must have the same `roxas_version` and distinct
`image_label` values. Where both objects contain the same entry, `x`
takes precedence:

- `$dataset` is taken from `x`,

- `$authors` are combined and deduplicated by `email`, then renumbered
  with the authors of `x` first; the contact person of `x` is kept,

- `$funding` is combined and deduplicated by entire row,

- `$related` is combined and deduplicated by `citation`,

- `$sites`, `$trees`, `$woodpieces` and `$slides` are combined and
  deduplicated by their ID column (with a warning for shared IDs with
  different values), and their counts are updated with
  [`update_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAmetadata.md).

## Usage

``` r
combine_QWAmetadata(x, y)
```

## Arguments

- x, y:

  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  objects

## Value

A combined
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object

## Details

Submission files only; TRIA dataset files are not edited.

## See also

[`combine_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAimages.md),
[`combine_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAdata.md),
[`update_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAmetadata.md)
