# Extract the data structure from file names

Extract the hierarchical structure of the data (i.e., which images
belong to which slide, woodpiece, tree, site) from the given file names
based on a provided regex labeling pattern. The extracted site, species,
tree, woodpiece, slide, image identifiers are joined by underscores to
derive unique codes for each level of the data structure for the
subsequent calculations (e.g. `tree_code = {site}_{species}_{tree}`).

Any custom labeling pattern may be used, provided that it allows to
uniquely identify each image and is consistently applied across all
files. The first example below assumes the standard labeling pattern
`{site}_{species}_{tree}_{slide}_{image}`, suggested in [Fonti et al.
(2025)](https://doi.org/10.3389/fpls.2025.1505389).

Not all hierarchical components are mandatory in the pattern, as long as
the images can be uniquely identified (e.g. if exactly one woodpiece was
extracted per tree, there might not be a specific woodpiece identifier,
or if all data is from the same site, there might not be a site
identifier). If all images pertain to the same site and/or the same
species, and these components are not part of the labeling pattern, they
may also be provided via the `site_label` and `species_code` input
parameters. Here, `species_code` should follow the standard ITRDB
species codes.

`extract_data_structure()` is a wrapper around
`get_structure_from_filenames()` that uses the data frame of ROXAS (AI)
files from
[`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md)
as input, applies the hierarchy extraction to the `$prefix` component,
and appends the original file paths as columns to the returned structure
data frame.

## Usage

``` r
get_structure_from_filenames(
  filenames,
  pattern,
  site_label = NULL,
  species_code = NULL
)

extract_data_structure(files, pattern, site_label = NULL, species_code = NULL)
```

## Arguments

- filenames:

  The vector of file names.

- pattern:

  The labeling pattern followed by the file names, a regex with named
  groups.

- site_label:

  Optional site code to be used if it is not part of the pattern, only
  if all images are from the same site.

- species_code:

  Optional species code to be used if it is not part of the pattern,
  only if all images are from the same species.

- files:

  The data frame of prefix and file paths returned by
  [`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md).

## Value

A data frame containing the extracted data structure (with the original
file names appended for `extract_data_structure()`).

## Examples

``` r
# the standard pattern {site}_{species}_{tree}_{slide}_{image}
pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
filenames <- c(
  "SITEA_PISY_01_1_1.jpg",
  "SITEA_PISY_01_1_2.jpg",
  "SITEA_PISY_01_2_1.jpg",
  "SITEB_LASI_02_1_1.jpg",
  "SITEB_LASI_03_1_1.jpg"
)
get_structure_from_filenames(filenames, pattern)
#> # A tibble: 5 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 2 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 3 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 4 SITEB_LASI_02_… SITEB_LASI… SITEB_LASI_02   SITEB_LAS… LASI         SITEB     
#> 5 SITEB_LASI_03_… SITEB_LASI… SITEB_LASI_03   SITEB_LAS… LASI         SITEB     
#> # ℹ 1 more variable: org_img_name <chr>

# custom pattern of the form {tree}-{slide}_{image}
filenames <- c(
 "tree1-sl1_img1.jpg",
 "tree1-sl2_img1.jpg",
 "tree2-sl1_img1.jpg",
 "tree2-sl1_img2.jpg"
 )
 pattern <- "(?<tree>[[:alnum:]].+)-(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
 get_structure_from_filenames(
   filenames, pattern,
   site_label = "SITEA", species_code = "LASI")
#> # A tibble: 4 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 2 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 3 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 4 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> # ℹ 1 more variable: org_img_name <chr>
```
