# Recover original labels from data structure

Inverse of extract_data_structure, i.e. given a dataframe with the data
structure (site, species, tree, woodpiece, slide, image), and the
labeling pattern followed by the original image filenames, we
reconstruct the filenames. This requires that all files follow the same
labeling pattern.

## Usage

``` r
reconstruct_img_names(df_structure, pattern)
```

## Arguments

- df_structure:

  The df with the data structure.

- pattern:

  The labeling pattern followed by the original image filenames, as a
  regex expression with named groups.

## Value

A list of reconstructed file names (without filetype extension)

## Details

NOTE: At the moment, only tested with following pattern:
`{site}_{species}_{tree}{woodpiece}_{slide}_{image}` where tree is
assumed to be a two-character code, and woodpiece is optional.
