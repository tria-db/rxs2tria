# Extract the data structure from input files

Extract the structure of the data (i.e., which images belong to which
slide, woodpiece, tree, site) from the filenames of the input data into
a dataframe. This requires that all files follow the same labeling
pattern. The extracted site, species, tree, woodpiece, slide, image
identifiers are joined by underscores to derive unique codes for each
level of the data structure for the subsequent calculations (e.g.
`tree_code = {site}_{species}_{tree}`).

## Usage

``` r
extract_data_structure(
  files,
  pattern =
    "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
  site_label = NULL,
  species_code = NULL
)

get_structure_from_filenames(
  filenames,
  pattern =
    "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
  site_label = NULL,
  species_code = NULL
)
```

## Arguments

- files:

  The list of vectors with input filenames.

- pattern:

  The labeling pattern followed by the original image filenames, as a
  regex with named groups.

- site_label:

  Optional site code to be used if it is not part of the pattern, only
  if all images are from the same site.

- species_code:

  Optional species code to be used if it is not part of the pattern,
  only if all images are from the same species.

- filenames:

  Vector of filenames to be processed.

## Value

A dataframe containing (the different filenames and) data structure.

## Details

For the default, we assume that all files contain the following pattern:
`{site}_{species}_{tree}{woodpiece}_{slide}_{image}`, where tree is a
two-character code, and woodpiece is optional, as suggested in [Fonti et
al. (2025)](https://doi.org/10.3389/fpls.2016.00781).

We already checked that the ROXAS cells, rings, settings and images
filenames match in
[`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md),
so it is ok to do the pattern extraction on the image filenames only.

The filenames should identify the site, species, tree, woodpiece, slide,
image such that each image file is uniquely identified by the
combination of these It's ok if not all components are present in the
pattern, as long as the images can be uniquely identified (e.g. if
exactly one woodpiece was extracted per tree, there might not be a
specific woodpiece identifier, or if all data is from the same site,
there might not be a site identifier in the name). So

- tree is a required components of the pattern

- site is optional iff there is only one site. In that case, provide the
  `site_label` as an input parameter

- woodpiece, slide and/or image are optional iff there is exactly one
  woodpiece per tree, resp. one slide per woodpiece, resp. one image per
  slide.

- species is optional. Can also be provided as an input parameter iff
  there is only one species.
