# Add supplementary resource files to a QWAmetadata object

Scans a directory and records the files it finds in the `$resources`
component of a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object. This is the convenient entry point for listing the supplementary
files (original and annotated images, raw ROXAS output, reference
series, etc.) you wish to submit alongside the required `QWAmetadata`
and `QWAdata` files.

## Usage

``` r
add_resources(x, path, recursive = FALSE, include_unmatched = FALSE)
```

## Arguments

- x:

  A
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object. Its `$images` component, if present, is used to auto-fill
  `linked_label` for `image`- and `analysis`-level files.

- path:

  Path to a directory to scan for files.

- recursive:

  If `TRUE`, recurse into sub-directories (default `FALSE`).

- include_unmatched:

  If `TRUE`, files that could not be matched to a specific type are kept
  with `resource_type = "other"` (default `FALSE`).

## Value

The
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object with the `$resources` component updated.

## Details

`add_resources()` is a thin wrapper around
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md):
it scans `path`, appends the newly found files to any resources already
stored in `x$resources`, and uses the object's own `$images` component
as the data structure for auto-filling `linked_label` on per-image
resources. Call it once per directory to build up the table across
several source folders.

Only a *listing* of files is produced — the files themselves are not
modified or copied. See
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md)
for the meaning of each column and
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for the full workflow, including how to review and complete
`linked_label` and the complete resource-type table.

## See also

[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md),
[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
QWA_meta <- add_resources(QWA_meta, path = "raw_roxas_files", recursive = TRUE)
# review / complete the table, then persist:
QWA_meta$resources
write_QWAmetadata(QWA_meta, "output_data/my_dataset_QWAmetadata.json")
} # }
```
