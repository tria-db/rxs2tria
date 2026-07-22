# Identify the ROXAS (AI) files in a directory

Returns a data frame with the full paths of the four types of relevant
ROXAS (AI) files (i.e., images, cell measurements, ring measurements,
and ROXAS settings resp. ROXAS AI metadata files) found within the given
directory. Recurses into any subdirectories except those explicitly
excluded. The files are identified by matching on the standard naming
suffixes produced by ROXAS resp. ROXAS AI, see the Details below.

## Usage

``` r
get_roxas_files(path_in, roxas_version, exclude_dirs = NULL)
```

## Arguments

- path_in:

  The path of the input directory.

- roxas_version:

  Which software was used to create the files, either `"roxas"` or
  `"roxas_ai"`.

- exclude_dirs:

  Optional character vector of subdirectory names that should not be
  searched for ROXAS (AI) files (e.g., `"unused_files"`).

## Value

A data frame with columns `prefix`, `fname_image`, `fname_settings`,
`fname_cells` and `fname_rings` containing the common prefixes and full
file paths.

## Details

For ROXAS, the required files per image are:

- The original image file (`{IMAGEID}.jpg` or `.jpeg`).

- The ROXAS settings file (`{IMAGEID}_ROXAS_Settings.txt`).

- The cell measurements output (`{IMAGEID}_Output_Cells.txt`).

- The ring measurements output (`{IMAGEID}_Output_Rings.txt`).

For ROXAS AI, the required files per image are:

- The original image file (`{IMAGEID}.scan.jpg` or `.jpeg`).

- The ROXAS AI metadata file (`{IMAGEID}.metadata.json`).

- The cell measurements table (`{IMAGEID}.cells_table.csv`).

- The ring measurements table (`{IMAGEID}.rings_table.csv`).
