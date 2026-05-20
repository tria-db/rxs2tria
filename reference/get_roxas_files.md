# Identify the ROXAS (AI) files in a directory

Returns a data frame with the full path names of the relevant ROXAS (AI)
files (i.e., images, cell measurements, ring measurements, and ROXAS
settings resp. ROXAS AI metadata files) found within the given
directory. The function recurses into any subdirectories, except those
explicitly excluded. The files are identified by matching on the
standard naming suffixes produced by ROXAS resp. ROXAS AI.

## Usage

``` r
get_roxas_files(path_in, roxas_version, exclude_dirs = NULL)
```

## Arguments

- path_in:

  path of the input directory.

- roxas_version:

  which ROXAS version was used to create the files (classic `"roxas"` or
  new AI version `"roxas_ai"`).

- exclude_dirs:

  directory names / keywords that should be ignored when searching for
  ROXAS files (e.g., "unused_files", optional)

## Value

A data frame with columns `prefix`, `fname_image`, `fname_settings`,
`fname_cells` and `fname_rings` containing the full file paths.

## Details

For ROXAS, the required files per image are:

- Image file (`{IMAGEID}.jpg` or `.jpeg`)

- Cells output (`{IMAGEID}_Output_Cells.txt`)

- Rings output (`{IMAGEID}_Output_Rings.txt`)

- Settings file (`{IMAGEID}_ROXAS_Settings.txt`)

For ROXAS AI, the required files per image are:

- Image file (`{IMAGEID}.scan.jpg` or `.jpeg`)

- Cells table (`{IMAGEID}.cells_table.csv`)

- Rings table (`{IMAGEID}.rings_table.csv`)

- Metadata file (`{IMAGEID}.metadata.json`)
