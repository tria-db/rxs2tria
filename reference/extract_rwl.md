# Extract an rwl series from ring or sector profile data

This function builds a dendrochronological dplR `rwl` object (a data
frame with years as row names and series IDs as column names) from
annually resolved QWA data, ready for scaling (see
[`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md))
and writing to `.rwl` with
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html).
Depending on the selected parameter, the function either uses ring-level
measurements (e.g. mean ring width, `mrw`) or profile-level measurements
at a given sector (aggregated cell parameters such as the 90th
percentile lumen area, `la_q90`).

Duplicate rings (`exclude_dupl`) and user-defined ring exclusions
(`exclude_issues`) are filtered out before constructing the final time
series. These flag columns are read from `df_rings`, making it a
required parameter, while `prf_data` (with the selected `sector`) is
only required to extract a `param` from the profile data.

Series are grouped by **woodpiece** (each core/sample yields one
series); thus the `woodpiece_label`s become the column names.

## Usage

``` r
extract_rwl(df_rings, param, prf_data = NULL, sector = NULL)
```

## Arguments

- df_rings:

  A data frame containing ROXAS ring-level measurements and logical flag
  columns (the `$rings` component of a `QWAdata` object, after calling
  [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
  to instantiate the flag columns).

- param:

  Character string specifying the parameter to export: either a
  measurement column in `df_rings`, or an aggregated cell measurement in
  `prf_data` (e.g. `"la_mean"`, `"la_q90"`).

- prf_data:

  A data frame containing ROXAS profile-level measurements (aggregated
  cell parameters via
  [`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)).
  Only required when `param` is not a `df_rings` column.

- sector:

  Integer specifying which sector to use when exporting a `prf_data`
  parameter. Required in that case, otherwise ignored.

## Value

A dplR `rwl` object with the selected `param` data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build an rwl object from mean ring width
extract_rwl(df_rings = QWA_data$rings,
           param = "mrw")

# Build an rwl object from a profile-level parameter
extract_rwl(df_rings = QWA_data$rings,
           param = "cwtrad_mean",
           prf_data = prf_sector,
           sector = 5)
} # }
```
