# Identify ROXAS data files

Given a path to a directory, returns a nested list with the full
filepaths of all required ROXAS files (images, cell outputs, ring
outputs, settings resp. metadata) found within (including all
subdirectories except those explicitly excluded). Note that this
function assumes that the files follow the standard naming suffixes form
ROXAS resp. ROXAS AI, and that each set of four files is stored in the
same (sub-)directory.

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

A named list of four character vectors (`fname_image`, `fname_cells`,
`fname_rings`, `fname_settings`) containing the full filepaths.
