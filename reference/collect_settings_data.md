# Read and combine ROXAS settings data

Collect the settings data from all ROXAS settings files

## Usage

``` r
collect_settings_data(files_settings, roxas_version = "classic", tz = NULL)
```

## Arguments

- files_settings:

  Vector/list of ROXAS settings filenames

- roxas_version:

  The version of ROXAS used to create the files (classic, AI)

## Value

A dataframe containing the extracted data.
