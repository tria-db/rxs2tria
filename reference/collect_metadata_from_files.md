# Read and combine raw metadata

Collect the ROXAS settings and image exif data from all raw files and
combine into one dataframe with the data structure.

## Usage

``` r
collect_metadata_from_files(df_structure, roxas_version = "classic", tz = NULL)
```

## Arguments

- df_structure:

  Dataframe with all input filenames

- roxas_version:

  ROXAS version (required to correctly extract info from settings files)

- tz:

  Timezone for date conversion of settings created at date (default:
  system tz)

## Value

A dataframe containing the extracted settings data.
