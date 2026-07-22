# Read and combine ROXAS (AI) settings and image metadata

Collect the TRIA-relevant attributes from a collection of ROXAS (AI)
settings and image EXIF metadata.

The settings (and, for ROXAS, image) file names can be supplied either
as a data frame `df` with the columns `fname_settings` (and
`fname_image`), such as the output of
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
or
[`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md),
or as the individual file path vectors `files_settings` (and
`files_images`). Exactly one of the two input styles must be used.

For ROXAS files, image EXIF metadata are extracted from the image files
and then joined to the settings data. For ROXAS AI files, the EXIF
metadata are already embedded in the settings JSON files and read from
there directly.

Therefore, `collect_settings_data()` requires the image file paths
(`files_images` or the `fname_image` column of `df`) as input if
`roxas_version = "roxas"`. When passed as a vector, they must be in the
same order as `files_settings`.

If `roxas_version` is not supplied, it is auto-detected from the
settings file names.

## Usage

``` r
collect_settings_data(
  df = NULL,
  files_settings = NULL,
  files_images = NULL,
  roxas_version = NULL
)
```

## Arguments

- df:

  Data frame with the file name columns `fname_settings` (and
  `fname_image` for ROXAS), e.g. from
  [`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md).
  Provide this or `files_settings`/`files_images`, but not both.

- files_settings:

  Vector of ROXAS settings file names.

- files_images:

  Vector of image file names in the same order as `files_settings`.
  Required when `roxas_version = "roxas"`.

- roxas_version:

  The version of ROXAS used to create the files, either `"roxas"` or
  `"roxas_ai"`. Auto-detected from the settings file names if not
  supplied.

## Value

A dataframe containing the extracted data.
