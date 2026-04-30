# Calculate sector-wise radial profiles

Divides each ring into `n_sectors` equal-width sectors (by relative
radial position) and computes per-sector means and optionally quantiles
of the selected cell parameters.

## Usage

``` r
calculate_sector_profiles(
  QWA_data,
  n_sectors,
  sel_cell_params,
  quant_probs = NULL
)
```

## Arguments

- QWA_data:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object with a non-`NULL` `$cells` component.

- n_sectors:

  Number of equal-width sectors to divide each ring into.

- sel_cell_params:

  Character vector of cell parameter column names to include in the
  profiles (e.g. `c("la", "cwttan")`).

- quant_probs:

  Numeric vector of quantile probabilities to calculate (e.g.
  `c(0.25, 0.5, 0.75)`). `NULL` or empty -\> means only.

## Value

A
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object with `profile_type = "sector"`.

## See also

[`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_band_profiles.md),
[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
