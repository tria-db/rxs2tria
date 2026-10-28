# Create a QWAmetadata object

`QWAmetadata` holds the hierarchical metadata associated with a QWA
dataset, structured by level of organisation:

- **`$dataset`**: dataset-level information (name, description,
  license).

- **`$authors`**: author information.

- **`$funding`**: funding sources and grant numbers.

- **`$related`**: related objects (publications, datasets, etc.).
  Submission files only.

- **`$datafiles`**: the cells and rings files. TRIA dataset files only.

- **`$sites`**: site-level metadata (location, climate, etc.).

- **`$trees`**: tree-level metadata (species, DBH, age, etc.).

- **`$woodpieces`**: woodpiece-level metadata (disc, core, etc.).

- **`$slides`**: slide-level metadata (preparation, staining, etc.).

- **`$images`**: image-level metadata, including the ROXAS output file
  paths, EXIF data, and ROXAS settings. This is the only required
  component and is usually populated automatically by
  [`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md).

All components except `images` are optional and can be filled in later
to prepare a complete dataset submission to the TRIA database.

The `submission` attribute
([`is_submission()`](https://tria-db.github.io/rxs2tria/reference/is_submission.md))
tells submission files from TRIA dataset files. It is inferred from the
components: TRIA dataset files have a `$datafiles` component or a
`ds_label` in `$dataset`. Components are aligned with the schema of
their stage; `$related` is dropped from TRIA dataset files with a
warning.

`as_QWAmetadata()` safely constructs a QWAmetadata object from a named
list. Component names are matched against the `QWAmetadata()`
constructor parameters; unknown names are dropped with a warning.

## Usage

``` r
QWAmetadata(
  dataset = NULL,
  authors = NULL,
  funding = NULL,
  related = NULL,
  datafiles = NULL,
  sites = NULL,
  trees = NULL,
  woodpieces = NULL,
  slides = NULL,
  images = data.frame()
)

as_QWAmetadata(x)
```

## Arguments

- dataset:

  Data frame with dataset-level metadata. Typically provided via
  metadata Shiny app
  ([`launch_metadata_app()`](https://tria-db.github.io/rxs2tria/reference/launch_metadata_app.md)).

- authors:

  Data frame with author information. Typically provided via metadata
  Shiny app.

- funding:

  Data frame with funding information. Typically provided via metadata
  Shiny app.

- related:

  Data frame with related publications or datasets. Typically provided
  via metadata Shiny app.

- datafiles:

  Data frame listing the cells and rings files of a TRIA dataset, with
  size, checksum and S3 key. Written by TRIA at publish time; leave
  `NULL` for submission files. (The supplementary CSV built by
  [`compile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/compile_supplementary.md)
  is a separate file, see
  [`vignette("supplementary")`](https://tria-db.github.io/rxs2tria/articles/supplementary.md).)

- sites:

  Data frame with site-level metadata. Typically provided via metadata
  Shiny app.

- trees:

  Data frame with tree-level metadata. Typically provided via metadata
  Shiny app.

- woodpieces:

  Data frame with woodpiece-level metadata. Typically provided via
  metadata Shiny app.

- slides:

  Data frame with slide-level metadata. Typically provided via metadata
  Shiny app.

- images:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object or plain data frame with image-level metadata. Required.
  Typically the output of
  [`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md).

- x:

  A named list with any subset of the `QWAmetadata()` components.

## Value

An object of class `QWAmetadata`.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md),
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md),
[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
