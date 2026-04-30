# Add a resources table to a QWAmetadata object

Stores a resources data frame (typically created by
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md))
as the `$resources` component of a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object. Appended to any previously stored resources, `$images` used to
infer linked labels.

## Usage

``` r
add_resources(x, path, recursive = FALSE, include_unmatched = FALSE)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object.

- path:

  Path to a directory to scan for files.

- recursive:

  If `TRUE`, recurse into sub-directories (default `FALSE`).

- include_unmatched:

  If `TRUE`, resources that could not be matched to a specific type are
  included as "other" (default `FALSE`).

## Value

The
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object with the `$resources` component updated.

## See also

[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md),
[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
