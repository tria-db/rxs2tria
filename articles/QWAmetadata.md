# The QWAmetadata class

``` r

library(rxs2tria)
```

## Overview

A `QWAmetadata` object is a named list that bundles the metadata of a
QWA dataset, either submission files or TRIA dataset files (see
[Submission files and TRIA dataset
files](#submission-files-and-tria-dataset-files)). Its components cover
the dataset, its authors and funding, and each level of the sampling
hierarchy:

| Component | Description |
|----|----|
| `$images` | Image-level metadata; a `QWAimages` object. **Required.** |
| `$sites` | Site-level metadata (name, coordinates, country, elevation, topography, soil, stand) |
| `$trees` | Tree-level metadata (species, DBH, height, age, status) |
| `$woodpieces` | Woodpiece-level metadata (organ, sample type, sampling year) |
| `$slides` | Slide-level metadata (preparation, staining, mounting) |
| `$dataset` | Dataset name, title, description, licence, acknowledgements; submission files add access rights and embargo date, TRIA dataset files add `ds_label`, `url`, `doi`, `citation`, `dataset_version` |
| `$authors` | Author names, affiliations, contact information |
| `$funding` | Funding sources and grant numbers |
| `$related` | Related publications or datasets (DOIs, URLs). Submission files only. |
| `$datafiles` | The cells and rings files of a TRIA dataset, with `size_bytes`, `checksum` (MD5) and `s3_key`. TRIA dataset files only. |

`$images` is the only required component. All others may be absent from
the object and can be populated incrementally, but a complete submission
needs all of them except `$related`. The hierarchical components
(`$sites`, `$trees`, `$woodpieces`, `$slides`) are linked to `$images`
via the shared label columns (`site_label`, `tree_label`,
`woodpiece_label`, `slide_label`).

------------------------------------------------------------------------

## Submission files and TRIA dataset files

The `submission` attribute, queried with
[`is_submission()`](https://tria-db.github.io/rxs2tria/reference/is_submission.md),
tells submission files (`TRUE`) from TRIA dataset files (`FALSE`). It is
inferred from the components: TRIA dataset files have `$datafiles` or a
`ds_label` in `$dataset`. Each component is aligned with and checked
against the schema of its stage. `$related` is dropped from TRIA dataset
files with a warning.
[`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md),
[`update_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAmetadata.md)
and
[`combine_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAmetadata.md)
apply to submission files only.

------------------------------------------------------------------------

## Building a QWAmetadata object

In normal use, `QWAmetadata` is assembled through the interactive
metadata Shiny app, which takes a `QWAimages` object as its starting
point and guides you through all components. The output is exported as a
JSON file that can be read back at any time:

``` r

launch_metadata_app()

QWA_metadata <- read_QWAmetadata("output/my_dataset_QWAmetadata.json")
```

You can also construct a `QWAmetadata` object directly, passing any
components you already have as data frames:

``` r

QWA_metadata <- QWAmetadata(
  images = rxs_images,      # required QWAimages object
  sites  = df_sites,        # optional data frames
  trees  = df_trees
)
```

Or from a named list, for example after reading from a custom source:

``` r

QWA_metadata <- as_QWAmetadata(my_list)
```

[`as_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
matches list names against the known component names, drops any
unrecognised names with a warning, and calls
[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
on the remainder.

The constructor aligns each supplied component with the JSON schema of
its stage: column names are cleaned, types are coerced, and unrecognised
columns are dropped. Missing required columns are added as `NA` with a
warning. `$images` goes through
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
(see
[`vignette("QWAimages")`](https://tria-db.github.io/rxs2tria/articles/QWAimages.md));
the other components are only aligned, not checked: run
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md)
before a submission.

------------------------------------------------------------------------

## Inspecting a QWAmetadata object

``` r

QWA_metadata          # same as print(QWA_metadata)
summary(QWA_metadata) # identical output
```

[`print()`](https://rdrr.io/r/base/print.html) shows the stage
(submission files, or TRIA dataset files with label, version and
citation), the image-level coverage (images, slides, woodpieces, trees,
sites, species, year range) and the status of each optional component —
present with row count, or flagged as not provided.

Individual components are standard data frames and can be accessed and
edited directly:

``` r

QWA_metadata$sites
QWA_metadata$authors

# Edit in place:
QWA_metadata$dataset$ds_license <- "CC BY 4.0"
```

------------------------------------------------------------------------

## Checking a QWAmetadata object

``` r

check_QWAmetadata(QWA_metadata)
```

[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md)
returns `TRUE` invisibly if no issue was found, `FALSE` otherwise. With
`strict = TRUE`, it aborts on the first issue instead of warning.

- **Errors**: invalid object; duplicate IDs in `$sites`, `$trees`,
  `$woodpieces` or `$slides`.
- **Warnings**:
  - a missing component required at the object’s stage;
  - values violating the schema of the stage (submission files or TRIA
    dataset files), and duplicate values in columns that must be unique
    (e.g. `email`, `site_name`);
  - not exactly one contact person, or `author_nr` not numbering the
    authors from 1 to n;
  - unknown country or species codes, or species columns of `$trees` not
    matching their code;
  - `[ADD INFO ...]` placeholders left in the dataset `description`;
  - IDs of `$images` missing from the hierarchical tables, extra IDs, or
    derived values (counts, parent labels, `species_code`) not matching
    `$images` (resync with `update_QWAmetadata(x, x$images)`);
  - issues in `$images`, see
    [`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md).
- **Information**: absent optional components and missing optional
  columns.

------------------------------------------------------------------------

## Completing a QWAmetadata object

``` r

QWA_metadata <- complete_QWAmetadata(QWA_metadata)
```

Returns a new `QWAmetadata` object with all optional columns added
(except `qc_input` columns set in the flags app) and existing values
preserved. Applies to submission files only.

- **`$images`**: delegates to
  [`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md).
- **Hierarchical tables** (`$sites`, `$trees`, `$woodpieces`,
  `$slides`): if a table is absent, it is pre-filled with the IDs and
  counts derived from `$images` (the same starting point the Shiny app
  uses), with all other columns initialised as `NA`. If a table is
  already present, only missing optional columns are added.
- **Flat tables**: if absent, `$dataset`, `$authors` and `$funding` get
  a one-row skeleton and `$related` an empty table, each with all schema
  columns, so you can see the expected fields. If already present, only
  missing optional columns are added.

------------------------------------------------------------------------

## Supplementary files

Supplementary files (original/annotated images, raw ROXAS output,
reference series, etc.) are not part of `QWAmetadata`. They are listed
in a separate supplementary CSV built with
[`compile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/compile_supplementary.md)
and submitted alongside a zip of the files themselves. See
[`vignette("supplementary")`](https://tria-db.github.io/rxs2tria/articles/supplementary.md)
for the full workflow.

------------------------------------------------------------------------

## Persisting to disk

`QWAmetadata` is saved as a JSON file, which preserves the nested list
structure and all component tables in a single file:

``` r

write_QWAmetadata(QWA_metadata, "output/my_dataset_QWAmetadata.json")

QWA_metadata <- read_QWAmetadata("output/my_dataset_QWAmetadata.json")
```

Partial submissions can be saved and reloaded at any time. The JSON
format is human-readable, so components can also be inspected or edited
outside of R if needed.

Empty values are written as `null`, and absent components are not
written. With `drop_empty = TRUE` (default), optional fields filled in
the apps that are empty in all rows are not written;
[`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md)
adds them back. `compress = TRUE` writes a `.json.gz`.
[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
infers the stage from the content.
