# Extract the data structure from file names

Extract the hierarchical structure of the data (i.e., which images
belong to which slide, woodpiece, tree, site) from the given file names
based on a provided regex labeling pattern. The extracted site, species,
tree, woodpiece, slide, image identifiers are joined by underscores to
derive unique codes for each level of the data structure for the
subsequent calculations (e.g. `tree_code = {site}_{species}_{tree}`).

The default pattern assumes that all file names follow the labeling
scheme `{site}_{species}_{tree}{woodpiece}_{slide}_{image}`, where
`{tree}` is a two-character code, and `{woodpiece}` is optional, as
suggested in [Fonti et al.
(2025)](https://doi.org/10.3389/fpls.2016.00781).

However, a different labeling pattern may be used, provided that it
allows to uniquely identify each image in the list of file names and is
consistently applied across all files. Not all hierarchical components
are mandatory in the pattern, as long as the images can be uniquely
identified (e.g. if exactly one woodpiece was extracted per tree, there
might not be a specific woodpiece identifier, or if all data is from the
same site, there might not be a site identifier in the name). Iff all
images pertain to the same site and/or the same species, and these
components are not part of the labeling pattern, they may also be
provided via the `site_label` and `species_code` input parameters. Here,
`species_code` should follow the standard ITRDB species codes, c.f.
[webapps.wsl.com/tria/#/about](https://webapps.wsl.com/tria/#/about).

`extract_data_structure()` is a wrapper around
`get_structure_from_filenames()` that uses the list of ROXAS files from
[`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md)
as input, applies the hierarchy extraction to the image file names, and
adds the original file names to the resulting dataframe.

## Usage

``` r
get_structure_from_filenames(
  filenames,
  pattern =
    "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
  site_label = NULL,
  species_code = NULL
)

extract_data_structure(
  files,
  pattern =
    "(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:][:alnum:])(?<woodpiece>[:alnum:]*)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)",
  site_label = NULL,
  species_code = NULL
)
```

## Arguments

- pattern:

  The labeling pattern followed by the file names, as a regex with named
  groups.

- site_label:

  Optional site code to be used if it is not part of the pattern, only
  if all images are from the same site.

- species_code:

  Optional species code to be used if it is not part of the pattern,
  only if all images are from the same species.

- files:

  The list of vectors with ROXAS file names.

## Value

A dataframe containing the extracted data structure (plus the original
file names for `extract_data_structure()`).

## Examples

``` r
# with the default pattern ({site}_{species}_{tree}{woodpiece}_{slide}_{image})
files <- c(
  "SITEA_PISY_01A_1_1.jpg",
  "SITEA_PISY_01A_1_2.jpg",
  "SITEA_PISY_01A_2_1.jpg",
  "SITEB_LASI_02A_1_1.jpg",
  "SITEB_LASI_02B_1_1.jpg"
)
get_structure_from_filenames(files)
#> # A tibble: 5 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01_A SITEA_PIS… PISY         SITEA     
#> 2 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01_A SITEA_PIS… PISY         SITEA     
#> 3 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01_A SITEA_PIS… PISY         SITEA     
#> 4 SITEB_LASI_02_… SITEB_LASI… SITEB_LASI_02_A SITEB_LAS… LASI         SITEB     
#> 5 SITEB_LASI_02_… SITEB_LASI… SITEB_LASI_02_B SITEB_LAS… LASI         SITEB     
#> # ℹ 1 more variable: org_img_name <chr>

# custom pattern of the form {tree}_{slide}_{image}
files <- c(
 "tree1_sl1_img1.jpg",
 "tree1_sl2_img1.jpg",
 "tree2_sl1_img1.jpg",
 "tree2_sl1_img2.jpg"
 )
 pattern <- "(?<tree>[:alnum:].+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)"
 get_structure_from_filenames(
   files, pattern,
   site_label = "SITEA", species_code = "PISY")
#> # A tibble: 4 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_PISY_tre… SITEA_PISY… SITEA_PISY_tre… SITEA_PIS… PISY         SITEA     
#> 2 SITEA_PISY_tre… SITEA_PISY… SITEA_PISY_tre… SITEA_PIS… PISY         SITEA     
#> 3 SITEA_PISY_tre… SITEA_PISY… SITEA_PISY_tre… SITEA_PIS… PISY         SITEA     
#> 4 SITEA_PISY_tre… SITEA_PISY… SITEA_PISY_tre… SITEA_PIS… PISY         SITEA     
#> # ℹ 1 more variable: org_img_name <chr>
```
