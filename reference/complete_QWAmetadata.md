# Complete a QWAmetadata object with all optional fields

Adds missing optional columns to `$images` (see
[`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md))
and to any already-present optional component tables. For absent
hierarchical tables (`$sites`, `$trees`, `$woodpieces`, `$slides`),
pre-fills known values from the hierarchy in `$images` (the same logic
used by the metadata Shiny app). For absent flat tables, inserts a
skeleton with all schema columns so the user can see the expected
fields: one empty row for `$dataset`, `$authors` and `$funding`, no rows
for `$related`.

## Usage

``` r
complete_QWAmetadata(x)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object.

## Value

A
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object with all optional columns and component tables initialized.

## Details

Existing tables are never overwritten; only missing optional columns are
added to them. Submission files only; TRIA dataset files are not edited.

## See also

[`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md),
[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
