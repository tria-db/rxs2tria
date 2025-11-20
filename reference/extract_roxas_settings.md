# Extract data from a ROXAS settings file

Helper function to read and extract the relevant metadata from a single
ROXAS settings file

## Usage

``` r
extract_roxas_settings(file_settings, roxas_version = "classic")
```

## Arguments

- file_settings:

  The file to be read.

- roxas_version:

  The version of ROXAS used to create the file (classic, AI)

## Value

A dataframe containing the extracted data.
