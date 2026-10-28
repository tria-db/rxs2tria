# Calculate radial profiles

`calculate_sector_profiles()` divides the cells of each ring into
`n_sectors` equal-width sectors (by relative radial position) and
computes per-sector means and optionally quantiles of the selected cell
parameters. The returned `QWAprofile` object includes a column
`sector_n` with values in \[1,..., `n_sectors`\] for each `image_label`
and `year`.

`calculate_band_profiles()` computes moving-band profiles of selected
cell parameters across the ring width. Each band is a window of
`bandwidth` microns that shifts in steps of `stepsize` microns from the
innner to the outer ring boundary. The returned `QWAprofile` object
includes columns `start` and `end` with the band limits in microns (with
the `mrw` value for context) for each `image_label` and `year`.

For each selected parameter, the returned columns include number of
valid cells used in the aggregation (`<param>_N`), the mean
(`<param>_mean`) for each sector or band, and the quantile columns for
the given probabilities (named by percentage, e.g. `la_q25`, `la_q05`,
`la_q25.7`).

## Usage

``` r
calculate_sector_profiles(
  QWA_data,
  n_sectors,
  sel_cell_params,
  quant_probs = NULL,
  apply_exclusions = FALSE,
  tissue = c("all", "affected")
)

calculate_band_profiles(
  QWA_data,
  bandwidth,
  stepsize,
  sel_cell_params,
  quant_probs = NULL,
  band_rebound = TRUE,
  apply_exclusions = FALSE,
  tissue = c("all", "affected")
)
```

## Arguments

- QWA_data:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object with a non-`NULL` `$cells` component (and `$rings` component
  for `calculate_band_profiles()`).

- n_sectors:

  Number of equal-width sectors to divide each ring into.

- sel_cell_params:

  Character vector of cell parameters to include in the profiles (either
  a subset of the measurement column names of `QWA_data$cells`, e.g.
  `c("la", "cwtrad")`, or `"all"`). NOTE: using `"all"` results in a
  large number of columns in the returned `QWAprofile`.

- quant_probs:

  Numeric vector of distinct quantile probabilities in \[0, 1\] (e.g.
  `c(0.25, 0.5, 0.75)`). Set `NULL` or empty to derive only means.

- apply_exclusions:

  If `TRUE`, the cells of excluded rings (`exclude_dupl`,
  `exclude_issues` in `$rings`) are filtered out, returning 'clean'
  measurement series ready for analyses. Default `FALSE`.

- tissue:

  Only used with `apply_exclusions = TRUE`. `"all"` (default) removes
  all cells of excluded rings; `"affected"` removes only the earlywood
  or latewood cells (by `ew_lw`) of rings with `exclude_issues` and
  `affected_tissue` `"ew"` or `"lw"`, respectively.

- bandwidth:

  Width of each band in microns.

- stepsize:

  Step size between band starts in microns.

- band_rebound:

  If `TRUE` (default), the last band is shifted so its end coincides
  exactly with the ring width (`mrw`).

## Value

A
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object with `profile_type = "sector"` or `profile_type = "band"`.

## Details

In order to use `apply_exclusions = TRUE`, the `QWA_data$rings`
component must be provided and include the flag columns `exclude_dupl`
and `exclude_issues`. In addition, to exclude only the cells from
affected tissue (via `tissue = "affected"`), the `ew_lw` assignment in
`$cells` must exist (see
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)).
The exclusion flag and `affected_tissue` columns can be edited via
[`launch_flags_app()`](https://tria-db.github.io/rxs2tria/reference/launch_flags_app.md)
before the profile calculation, if you want a 'clean' profile object
that does not include rows from overlapping or problematic rings (or
tissue).

To return a `ew_band` column in a band-type `QWAprofile` (indicating if
a band falls entirely within the earlywood), the `$rings` component must
include an `eww` column, which can be added via
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md).

## See also

[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
