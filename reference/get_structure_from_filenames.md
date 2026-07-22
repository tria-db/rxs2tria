# Extract the hierarchical data structure from ROXAS file names

Extract the hierarchical structure of the data (i.e., which images
belong to which slide, woodpiece, tree, site) from the given file names
based on a provided regex labeling pattern. The extracted site, species,
tree, woodpiece, slide, image identifiers are joined by underscores to
derive unique codes for each level of the data structure for the
subsequent calculations (e.g. `tree_code = {site}_{species}_{tree}`).

Any custom labeling pattern may be used, provided that it allows to
uniquely identify each image and is consistently applied across all
files. The first example below assumes the standard labeling pattern
`{site}_{species}_{tree}_{slide}_{image}`, proposed by [Fonti et al.
(2025)](https://doi.org/10.3389/fpls.2025.1505389).

Not all hierarchical components are mandatory in the pattern, as long as
the images can be uniquely identified (e.g. if exactly one woodpiece was
extracted per tree, tree and woodpiece identifiers might be identical—in
that case, capture the highest possible hierarchical level in the
pattern). If all images pertain to the same site and/or the same
species, the site label or species code can alternatively be provided as
input parameters. A site identifier (via pattern or site_label) is
always required, while the species code is optional. It is recommended
to use the ITRDB species codes.

`extract_data_structure()` is a wrapper around
`get_structure_from_filenames()` that uses the data frame of ROXAS (AI)
files from
[`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md)
as input, applies the hierarchy extraction to the column `prefix`, and
appends the original file paths as columns to the returned structure
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

  The character vector of file names or prefixes.

- pattern:

  A regex with named groups (site, species, tree, slide and/or image) to
  describe the labeling pattern of the `filenames`.

- site_label:

  Optional site label to be used if it is not part of the `pattern`,
  only if all images are from the same site.

- species_code:

  Optional species code to be used if it is not part of the `pattern`,
  only if all images are from the same species.

- files:

  The data frame of prefixes and file paths returned by
  [`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md).

## Value

A data frame containing the extracted data structure (with the original
file name columns appended for `extract_data_structure()`).

## Examples

``` r
# the common pattern {site}_{species}_{tree}_{slide}_{image}
pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
filenames <- c(
  "path/to/file/SITEA_PISY_01_1_1.jpg",
  "path/to/file/SITEA_PISY_01_1_2.jpg",
  "path/to/file/SITEA_PISY_01_2_1.jpg",
  "path/to/file/SITEB_LASI_02_1_1.jpg",
  "path/to/file/SITEB_LASI_03_1_1.jpg"
)
get_structure_from_filenames(filenames, pattern)
#> ✔ Identified 5 images from 4 slides > 3 woodpieces > 3 trees of 2 species at 2
#>   sites.
#> ℹ The 3 derived woodpiece labels are:
#>   SITEA_PISY_01
#>   SITEB_LASI_02
#>   SITEB_LASI_03
#> # A tibble: 5 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 2 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 3 SITEA_PISY_01_… SITEA_PISY… SITEA_PISY_01   SITEA_PIS… PISY         SITEA     
#> 4 SITEB_LASI_02_… SITEB_LASI… SITEB_LASI_02   SITEB_LAS… LASI         SITEB     
#> 5 SITEB_LASI_03_… SITEB_LASI… SITEB_LASI_03   SITEB_LAS… LASI         SITEB     
#> # ℹ 1 more variable: org_img_name <chr>

# pattern of the form {tree}-{slide}_{image}
# NOTE: filenames without full paths or extension also work
filenames <- c(
 "tree1-sl1_img1",
 "tree1-sl2_img1",
 "tree2-sl1_img1",
 "tree2-sl1_img2"
 )
 pattern <- "(?<tree>[[:alnum:]].+)-(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"
 get_structure_from_filenames(
   filenames, pattern,
   site_label = "SITEA", species_code = "LASI")
#> ✔ Identified 4 images from 3 slides > 2 woodpieces > 2 trees of 1 species at 1
#>   site.
#> ℹ The 2 derived woodpiece labels are:
#>   SITEA_LASI_tree1
#>   SITEA_LASI_tree2
#> # A tibble: 4 × 7
#>   image_label     slide_label woodpiece_label tree_label species_code site_label
#>   <chr>           <chr>       <chr>           <chr>      <chr>        <chr>     
#> 1 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 2 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 3 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> 4 SITEA_LASI_tre… SITEA_LASI… SITEA_LASI_tre… SITEA_LAS… LASI         SITEA     
#> # ℹ 1 more variable: org_img_name <chr>
```
