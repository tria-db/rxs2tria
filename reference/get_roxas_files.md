# Identify ROXAS data files

Given a path to a directory, returns a nested list with the full
filepaths of all required ROXAS files (images, cell outputs, ring
outputs, settings) found within (including subdirectories).

## Usage

``` r
get_roxas_files(path_in)
```

## Arguments

- path_in:

  path of the input directory.

## Value

A list of lists containing filepaths of images and ROXAS output files
(cells, rings, settings).
