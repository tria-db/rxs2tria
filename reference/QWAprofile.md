# Create a QWAprofile object

`QWAprofile` is a subclass of `data.frame` that holds profiles of QWA
measurements aggregated along the annual rings, computed from a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object with
[`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)
or
[`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md).

## Usage

``` r
QWAprofile(data, profile_type = c("sector", "band"))
```

## Arguments

- data:

  A data frame containing the profile data. Must include at minimum
  columns identifying the image/ring (`image_label`, `year`) and the bin
  position.

- profile_type:

  The binning scheme used: `"sector"` (relative position, default) or
  `"band"` (fixed-width distance bands).

## Value

An object of class `c("QWAprofile", "data.frame")` with a `profile_type`
attribute.

## Details

Two profile types are supported, set via the `profile_type` attribute:

- **`"sector"`**: bins defined by equal relative position sectors across
  the ring width (0–100%).
  [`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)

- **`"band"`**: bins defined by fixed-width distance bands in microns.
  [`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)

## See also

[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md),
[`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)
