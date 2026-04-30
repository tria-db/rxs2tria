# Collect resource file information from a directory

Scans a directory for files and returns a data frame listing each file
with its inferred resource type. Optionally appends to an existing
resources data frame, making it easy to build a resource list
incrementally from multiple directories (e.g., processed outputs + raw
ROXAS files + images).

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
  to it.

- df_structure:

  Optional data frame with label columns (e.g. from
  [`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)).
  Reserved for future use to auto-populate `linked_label` from matching
  file names.

- recursive:

  If `TRUE`, recurse into sub-directories (default `FALSE`).

- include_unmatched:

  If `TRUE`, resources that could not be matched to a specific type are
  included as "other" (default `FALSE`).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- `resource_name`: base file name.

- `resource_type`: inferred resource type string.

- `linked_level`: default hierarchy level for this type (`"dataset"`,
  `"woodpiece"`, `"slide"`, `"image"`, or `"analysis"`).

- `linked_label`: label of the linked entity (`NA`; fill in manually).

- `fname_resource`: absolute file path.

## Details

Resource types are inferred from file names via `infer_resource_type()`.
See that function's documentation for the full pattern table.

## See also

[`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
