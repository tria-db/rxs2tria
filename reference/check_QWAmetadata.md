# Check a QWAmetadata object

Checks each component of a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object, as the final check of the metadata before a submission. Aborts
if the object is not a valid `QWAmetadata` or if IDs in `$sites`,
`$trees`, `$woodpieces` or `$slides` are not unique. Warns (or, with
`strict = TRUE`, aborts) on:

- a missing component required at the object's stage (see
  [`is_submission()`](https://tria-db.github.io/rxs2tria/reference/is_submission.md));

- values violating the JSON schema of the stage;

- duplicate values in columns that must be unique (e.g. `email`,
  `site_name`);

- not exactly one contact person, or `author_nr` not numbering the
  authors from 1 to n;

- country codes not in ISO 3166-1 alpha-2;

- species codes not in the ITRDB list, or species columns of `$trees`
  not matching their code;

- template placeholders left in the dataset `description`;

- IDs or derived values (counts, parent labels, `species_code`) in the
  hierarchical tables not matching `$images` (resync with
  [`update_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAmetadata.md));

- issues in `$images`, see
  [`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md).

## Usage

``` r
check_QWAmetadata(x, strict = FALSE)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object.

- strict:

  If `TRUE`, abort on the first issue instead of warning.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## Details

Also informs about missing optional columns and absent optional
components.

## See also

[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md),
[`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md),
[`check_consistency()`](https://tria-db.github.io/rxs2tria/reference/check_consistency.md)
