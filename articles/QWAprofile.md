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

`sel_cell_params` selects the cell measurement columns to aggregate
(`measure` and `derived` columns of `$cells`, excluding positions,
`sector100`, `ew_lw` and `nbrid`), or `"all"`. `quant_probs` is
optional; omitting it produces counts and means only. Band profiles skip
rings narrower than `bandwidth` or without `mrw`.

With `apply_exclusions = TRUE`, the cells of excluded rings
(`exclude_dupl`, `exclude_issues`; `NA` counts as `FALSE`) are removed
before aggregating. With `tissue = "affected"`, rings with
`exclude_issues` and `affected_tissue` `"ew"` or `"lw"` lose only the
cells of that tissue (requires `ew_lw`).

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
`{param}_mean`, `{param}_N`, and optionally `{param}_q{pct}` for each
quantile, with `pct` the percentage as in
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)
(e.g. `la_mean`, `la_N`, `la_q05`, `la_q50`, `la_q25.7`). Band profiles
also include `mrw` and, if available, `eww` and the `ew_band` flag
indicating whether a bin falls within the earlywood.

------------------------------------------------------------------------

## Checking a QWAprofile object

``` r

check_QWAprofile(prf_sector)
```

[`check_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/check_QWAprofile.md)
checks whether a profile is ready for analysis (profiles are derived
data and not part of a TRIA submission). It returns `TRUE` invisibly if
no issue was found, `FALSE` otherwise; with `strict = TRUE`, it aborts
on the first issue instead of warning.

- **Errors**: invalid object, i.e. `image_label`, `year` or the bin
  columns (`sector_n` for sector profiles, `start`/`end` for band
  profiles) missing, mistyped, `NA` or not unique; bands with
  `start >= end`.
- **Warnings**: no measurement columns, or all measurement values `NA`;
  bands of different widths; `sector_n` \< 1.

To check that a profile matches the data it was computed from, use
`check_consistency(data = QWA_data, profile = prf_sector)`.

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
