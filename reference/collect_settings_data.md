# Read and combine ROXAS settings data

Collect the settings data from all ROXAS settings files.

## Usage

``` r
collect_settings_data(files_settings, files_images = NULL, roxas_version)
```

## Arguments

- files_settings:

  Vector/list of ROXAS settings filenames.

- files_images:

  Vector/list of image filenames in the same order as `files_settings`.
  Required when `roxas_version = "roxas"`.

- roxas_version:

  The version of ROXAS used to create the files.

## Value

A dataframe containing the extracted data.

## Details

For `roxas_version = "roxas"`, image EXIF metadata is collected from the
image files via
[`collect_image_info()`](https://tria-db.github.io/rxs2tria/reference/collect_image_info.md)
and joined to the settings data. `files_images` is required in this case
and must be in the same order as `files_settings`.

For `roxas_version = "roxas_ai"`, image metadata is already embedded in
the settings JSON files; `files_images` is not needed.
