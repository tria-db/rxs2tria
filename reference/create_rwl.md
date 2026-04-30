# Create a Tucson (.rwl) file from ROXAS ring-width or profile data

This function creates a dendrochronological Tucson-format (`.rwl`) file
from ROXAS output data. Depending on the selected parameter, the
function either uses ring-level measurements (e.g. mean ring width,
`mrw`) or profile-level measurements aggregated at a given sector.

## Usage

``` r
create_rwl(
  prf_data,
  df_rings,
  PAR = "mrw",
  SECTOR = NULL,
  path_out,
  remove_excluded = TRUE
)
```

## Arguments

- prf_data:

  A data frame containing ROXAS profile-level measurements (e.g.
  sector-based anatomical parameters).

- df_rings:

  A data frame containing ROXAS ring-level measurements and logical flag
  columns (default: `QWA_data$rings`).

- PAR:

  Character string specifying the parameter to export. Use `"mrw"` for
  mean ring width, or any valid column name present in `prf_data` for
  profile-level parameters.

- SECTOR:

  Integer specifying which sector to use when exporting profile-level
  parameters (default: 5).

- path_out:

  Character string specifying the output directory where the `.rwl` file
  and mapping information will be written.

- remove_excluded:

  should rings with issues be removed?

## Value

The function is called for its side effects. It writes a Tucson-format
`.rwl` file to disk and returns the final data frame invisibly.

## Details

Duplicate rings and user-defined exclusions are automatically removed
before constructing the final time series.

The function operates in two modes:

- **Ring-level mode** (`PAR == "mrw"`): Ring-width values are taken from
  `df_rings`. Rings flagged as duplicated (`exclude_issues == TRUE`) are
  removed. If multiple measurements exist for the same year and image,
  values are averaged. Ring widths are converted from micrometers to
  millimeters before export.

- **Profile-level mode** (`PAR != "mrw"`): Measurements are taken from
  `prf_data` for the selected sector. Years flagged either as duplicated
  rings or excluded via `exclude_scope` are removed prior to
  aggregation.

Image identifiers are simplified to tree-level series IDs before
exporting, and the resulting data are written to a Tucson `.rwl` file
using
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an RWL file from mean ring width
create_rwl(prf_data = prf_data,
           df_rings = QWA_data$rings,
           PAR = "mrw",
           path_out = tempdir())

# Create an RWL file from a profile-level parameter
create_rwl(prf_data = prf_data,
           PAR = "cwtrad_mean",
           df_rings = QWA_data$rings, #df_rings = read.csv(paste0(path_out, "/", "20251230_TRIA_YAM_AI_1880_rings_edited.csv"))
           SECTOR = 5,
           path_out = tempdir())
} # }
```
