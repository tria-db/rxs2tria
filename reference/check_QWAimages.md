# Check a QWAimages object

Checks a
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object before submission; also run by
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md).
Aborts if the object is not a valid `QWAimages` (structure labels
missing or `NA`, duplicate `image_label`s, `$software` not matching the
`roxas_version` attribute). Warns (or, with `strict = TRUE`, aborts) on:

- values violating the JSON schema of the given stage;

- an unsound data structure: labels not starting with their parent
  label, or a label in more than one parent;

- `outmost_year` in the future;

- missing `spatial_resolution`.

## Usage

``` r
check_QWAimages(x, submission = TRUE, strict = FALSE)
```

## Arguments

- x:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object.

- submission:

  Check against the schema of submission files (`TRUE`) or TRIA dataset
  files (`FALSE`)? Set by
  [`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md).

- strict:

  If `TRUE`, abort on the first issue instead of warning.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## Details

Also informs about missing optional columns and empty values, except for
the source file names (`fname_*`, only needed during processing) and the
often incomplete EXIF data (`img_created_at`, `img_software`).

## See also

[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md),
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md),
[`check_consistency()`](https://tria-db.github.io/rxs2tria/reference/check_consistency.md)
