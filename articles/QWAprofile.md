# The QWAprofile class

``` r

library(rxs2tria)
```

## Overview

A `QWAprofile` object is a tibble subclass — one row per position bin
per ring per image — that holds cell measurements aggregated along the
radial direction of each annual ring. It carries a single attribute,
`profile_type`, which is either `"sector"` or `"band"` and determines
how bins are defined.

`QWAprofile` is computed from a `QWAdata` object and is independent of
the `QWAimages` or `QWAmetadata` objects. The link to the broader
dataset is the `image_label` column.

------------------------------------------------------------------------

## Profile types

Two binning schemes are available, chosen based on whether relative or
absolute position within the ring matters:

**Sector profiles** divide each ring into *n* bins of equal *relative*
width (0–100% of ring width). Because they are relative, sectors are
comparable across rings of different absolute widths and are appropriate
for most anatomical analyses.

**Band profiles** use a sliding window of fixed *absolute* width and
step size (in µm). They are suited to analyses where absolute radial
distance from the cambium is meaningful — for example, density gradients
in the first *n* µm of earlywood.

------------------------------------------------------------------------

## Computing a QWAprofile

``` r

prf_sector <- calculate_sector_profiles(
  QWA_data,
  n_sectors      = 5,
  sel_cell_params = c("la", "cwttan"),
  quant_probs    = c(0.1, 0.5, 0.9)
)
```

``` r

prf_band <- calculate_band_profiles(
  QWA_data,
  bandwidth       = 50,
  stepsize        = 25,
  sel_cell_params = c("la", "cwttan"),
  quant_probs     = c(0.1, 0.5, 0.9)
)
```

`sel_cell_params` selects which cell-level columns from `QWA_data$cells`
to aggregate. `quant_probs` is optional; omitting it produces means
only.

------------------------------------------------------------------------

## What the object contains

``` r

summary(prf_sector)
```

[`summary()`](https://rdrr.io/r/base/summary.html) reports the profile
type, dimensions, image count, and year range.
[`print()`](https://rdrr.io/r/base/print.html) prints the underlying
tibble.

Columns fall into three groups:

**Identifiers**: `image_label` and `year` (present in both types).

**Bin position**: - Sector profiles: `sector_n` (integer, 1 to
`n_sectors`) - Band profiles: `start` and `end` (bin boundaries in µm)

**Measurements**: one or more columns per selected cell parameter, named
`{param}_mean`, `{param}_N`, and optionally `{param}_q{prob}` for each
quantile (e.g. `la_mean`, `la_N`, `la_q10`, `la_q50`, `la_q90`). Sector
profiles may also include `mrw`, `eww`, and the `ew_band` flag
indicating whether a bin falls within the earlywood.

------------------------------------------------------------------------

## Adding parameters to an existing profile

If you need to add cell parameters that were not included when the
profile was first computed, use
[`complete_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAprofile.md)
rather than recomputing from scratch. It reuses the original bin
definitions and only computes the parameters not already present:

``` r

prf_sector <- complete_QWAprofile(
  prf_sector,
  QWA_data   = QWA_data,
  add_params = c("drad", "dtan"),
  quant_probs = c(0.1, 0.5, 0.9)   # should match the original
)
```

For band profiles,
[`complete_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAprofile.md)
infers `bandwidth` and `stepsize` from the existing `start`/`end`
columns.

------------------------------------------------------------------------

## Checking a QWAprofile object

``` r

check_QWAprofile(prf_sector)
```

[`check_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/check_QWAprofile.md)
returns `TRUE` invisibly and issues warnings for:

- Missing or `NA`-containing identifier columns (`image_label`, `year`).
- Missing bin column (`sector_n` for sector profiles; `start`/`end` for
  band profiles), or invalid band definitions where `start >= end`.
- No measurement columns present.

------------------------------------------------------------------------

## Persisting to disk

``` r

write_QWAprofile(prf_sector, "output/my_dataset_QWAprofile_sector5.csv.gz")

prf_sector <- read_QWAprofile("output/my_dataset_QWAprofile_sector5.csv.gz")
```

[`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
saves the tibble as a CSV. The `profile_type` attribute is **not**
stored in the file — it is re-derived from the column names when read
back: presence of `sector_n` → `"sector"`; presence of both `start` and
`end` → `"band"`. If neither is found,
[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md)
throws an error.

If `compress = TRUE` and the file name does not already end in `.gz`,
the extension `.gz` is appended — consistent with the other `write_*`
functions in the package.
