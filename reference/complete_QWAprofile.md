# Add cell parameters to an existing QWAprofile object

Computes profiles for additional cell parameters from `QWA_data` and
merges them into an existing
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object. Only parameters not already present in `x` are computed. The bin
definitions (sectors or bands) and aggregation settings from the
original profile are reused.

## Usage

``` r
complete_QWAprofile(x, QWA_data, add_params, quant_probs = NULL)
```

## Arguments

- x:

  A
  [QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  object to extend.

- QWA_data:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object (source of cell measurements).

- add_params:

  Character vector of additional cell parameter column names to compute
  and add.

- quant_probs:

  Numeric vector of quantile probabilities. Should match those used when
  computing `x`. `NULL` adds means only.

## Value

A
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
object with the new parameter columns appended.

## See also

[`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md),
[`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_band_profiles.md),
[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
