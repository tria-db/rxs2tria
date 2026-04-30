# Calculate band-wise radial profiles

Computes moving-band profiles of selected cell parameters across the
ring width. Each band is a window of `bandwidth` microns that shifts in
steps of `stepsize` microns from the cambial side to the lumen side of
the ring.

## Usage

``` r
calculate_band_profiles(
  QWA_data,
  bandwidth,
  stepsize,
  sel_cell_params,
  quant_probs = NULL,
  band_rebound = TRUE
)
```

## Arguments

- QWA_data:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object with non-`NULL` `$cells` and `$rings` components.

- bandwidth:

  Width of each band in microns.

- stepsize:

  Step size between band starts in microns.

- sel_cell_params:

  Character vector of cell parameter column names to include in the
  profiles (e.g. `c("la", "cwttan")`).

- quant_probs:

  Numeric vector of quantile probabilities to calculate (e.g.
  `c(0.25, 0.5, 0.75)`). If not provided, only means will be calculated.

- band_rebound:

  If `TRUE` (default), the last band is shifted so its end coincides
  exactly with the ring width (`mrw`).

## Value

A
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object with `profile_type = "band"`.

## Details

Requires that
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
has been run, as `$rings` must contain `mrw` and `eww`. `raddistr.st` is
computed internally from `rraddistr` and `mrw`.

## See also

[`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md),
[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
