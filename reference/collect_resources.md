# Collect resource file information from a directory

Scans a directory for files and returns a data frame ("resources table")
listing each file together with its inferred resource type, the
hierarchy level and entity it belongs to, and a checksum and size used
for integrity checking during submission. Files can be collected
incrementally from several directories by passing the previous result to
`append_to`, so a single table can describe processed outputs, raw ROXAS
files, and images that live in different folders.

## Usage

``` r
collect_resources(
  path,
  append_to = NULL,
  df_structure = NULL,
  recursive = FALSE,
  include_unmatched = FALSE
)
```

## Arguments

- path:

  Path to a directory to scan for files.

- append_to:

  Optional resources data frame from a previous call to
  `collect_resources()`. When provided, the new resources are appended
  to it (validated against the resources schema first).

- df_structure:

  Optional data frame with the label columns `org_img_name` and
  `image_label` (e.g. from
  [`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
  or the `$images` component of a
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object). Used to auto-populate `linked_label` for `image`- and
  `analysis`-level resources.

- recursive:

  If `TRUE`, recurse into sub-directories (default `FALSE`).

- include_unmatched:

  If `TRUE`, files that could not be matched to a specific type are kept
  with `resource_type = "other"` (default `FALSE`, i.e. such files are
  dropped).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per file and columns:

- `resource_name`: base file name.

- `resource_type`: inferred resource type string (see Details).

- `linked_level`: default hierarchy level for this type (`"dataset"`,
  `"woodpiece"`, `"slide"`, `"image"`, or `"analysis"`).

- `linked_label`: label of the linked entity, auto-filled from
  `df_structure` where possible, otherwise `NA` (fill in manually).

- `fname_resource`: absolute path to the file on your machine.

- `checksum`: MD5 checksum of the file contents.

- `size_bytes`: file size in bytes.

## Details

This function produces a *manifest* of individual files. It does not
move, copy, rename, or compress anything — the files stay where they are
and are only referenced by their path in `fname_resource`. How the
listed files are ultimately packaged and stored (e.g. grouped into
archives in the TRIA storage bucket) is handled downstream and does not
need to be described here.

See
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for a worked example and the complete list of recognised resource types.

## Resource types

The `resource_type` of each file is inferred from its name by matching
it against the known ROXAS, ROXAS AI, and QWA naming conventions (for
example `*_Output_Cells.txt` becomes `"roxas_output_cells"`,
`*.metadata.json` becomes `"rai_metadata"`). Patterns are checked
most-specific first. Files that match no known pattern are typed as
`"other"` and, unless `include_unmatched = TRUE`, dropped from the
result. Known backup and junk files (ROXAS `_bu` backups, `Thumbs.db`,
Office lock files) are always excluded. The full pattern table is
documented in
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md).

## Hierarchy level and linked entity

Every resource type has a default `linked_level` describing the level of
the data hierarchy the file pertains to: `"dataset"` (applies to the
whole submission, e.g. a reference chronology), `"woodpiece"`,
`"slide"`, `"image"`, or `"analysis"` (per-image ROXAS analysis files
such as shapefiles or annotated images). `linked_label` identifies
*which* entity at that level the file belongs to (e.g. a specific
`image_label`).

`linked_label` is filled automatically for `image`- and `analysis`-level
resources when `df_structure` is supplied (see below); all other rows
start as `NA` and should be reviewed and completed manually where
relevant.

## Automatic label matching

When `df_structure` (a data structure table from
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md),
or the `$images` component of a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object) is provided, each unlabelled `image`/`analysis` resource is
matched to an image by testing whether its file name starts with an
image's base name. The longest matching base name wins, which prevents
false matches between labels that share a prefix (e.g. `S22_L1` vs
`S22_L10`). Resources that cannot be matched are left as `NA`, and the
number of successful matches is reported.

## See also

[`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
to attach the result to a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object;
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for the full workflow and resource-type table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Collect the processed outputs, then append raw ROXAS files and images
# scanned recursively from a second directory:
res <- collect_resources("path/to/output_data")
res <- collect_resources("path/to/raw_roxas_files",
                         append_to = res, recursive = TRUE)

# Auto-fill linked_label for per-image resources using a data structure:
res <- collect_resources("path/to/raw_roxas_files",
                         df_structure = my_structure, recursive = TRUE)
} # }
```
