# The QWAdata class

``` r

library(rxs2tria)
```

## Overview

A `QWAdata` object is a named list with two components:

- **`$cells`** — cell-level measurements, one row per cell.
- **`$rings`** — ring-level measurements and quality flags, one row per
  annual ring per image.

At least one component must be present; the other may be `NULL`. The
object does not carry ROXAS settings or hierarchical metadata — those
stay in the accompanying `QWAimages` or `QWAmetadata` object, linked to
`QWAdata` via the `image_label` key.

------------------------------------------------------------------------

## Building a QWAdata object

In normal use, `QWAdata` is built incrementally in three steps:

``` r

# Step 1: read raw ROXAS output into a QWAdata object
QWA_data <- collect_raw_data(rxs_images)

# Step 2: add derived measures and ring quality flags
QWA_data <- complete_QWAdata(QWA_data, meta = rxs_images,
                              exclude_mode = "either")

# Step 3: validate
check_QWAdata(QWA_data, meta = rxs_images)
```

[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
reads the cells and rings tables from all ROXAS output files referenced
in a `QWAimages` object, cleans invalid values (negatives, error codes →
`NA`), and returns a minimal `QWAdata` with raw measurements only.

[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
then runs the full preprocessing pipeline (see [Completing a QWAdata
object](#completing-a-qwadata-object) below) and
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
validates the result.

You can also construct a `QWAdata` object directly from existing data
frames, for example after importing from another source:

``` r

QWA_data <- QWAdata(cells = df_cells, rings = df_rings)
```

The constructor enforces minimal column requirements (`image_label`,
`year`, `xpix`, `ypix` in cells; `image_label`, `year` in rings),
coerces column types, fills any year gaps in the rings sequence, and
checks for dating problems (duplicate years, future years). It warns if
`cwttan` is all-`NA` for any image, which indicates missing cell wall
thickness estimates — expected for conifers, acceptable for angiosperms.

------------------------------------------------------------------------

## What the object contains

``` r

QWA_data          # same as print(QWA_data): compact overview
summary(QWA_data) # identical output
```

[`print()`](https://rdrr.io/r/base/print.html) reports which components
are present and their dimensions, the number of woodpieces, slides, and
images, the year range, and — once the flag columns have been added — a
count of incomplete, missing, and duplicate rings.

### `$cells`

One row per cell. Columns fall into three groups:

**Identifiers**: `image_label` (links to `QWAimages`), `year`, and pixel
coordinates `xpix` / `ypix`.

**Raw measurements**: cell dimensions and wall thickness directly from
ROXAS — lumen dimensions (`drad`, `dtan`), radial distribution
(`rraddistr`), lumen area (`la`), cell wall areas (`cwa`), and cell wall
thicknesses (`cwtrad`, `cwttan`, `cwtall`, `rtsr`).

**Derived measures** (added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
`tca`, `rwd2`, `dcwt`, `cwtall.adj`, `cdrad`, `cdtan`, `cdratio`,
`sector100`, `ew_lw` (earlywood / latewood classification based on Mork
index).

### `$rings`

One row per ring per image. Columns fall into three groups:

**Identifiers**: `image_label`, `year`, `woodpiece_label`,
`slide_label`, and `cno` (cell count per ring, derived from `$cells`).

**Measurements**: mean ring width (`mrw`), ring area (`ra`), and after
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md),
earlywood and latewood widths (`eww`, `lww`).

**Quality flags** (added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
logical columns described in the next section.

------------------------------------------------------------------------

## Completing a QWAdata object

``` r

QWA_data <- complete_QWAdata(QWA_data, meta = rxs_images,
                              exclude_mode = "either")
```

[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
runs three operations in sequence and returns a new `QWAdata` object:

1.  **Derived cell measures** — computes the derived columns listed
    above that are not yet present in `$cells`. Already-present columns
    are skipped, so re-running is safe.

2.  **Year sequence completion** — extends `$rings` to cover every year
    in the cells data (filling gaps with `cno = 0` and `FALSE` for flag
    columns), and checks that no ring year exceeds `outmost_year` from
    `meta`.

3.  **Ring quality flags** — adds five logical columns to `$rings`:

    | Flag | Description |
    |----|----|
    | `incomplete_ring` | Ring at the inner or outer image border; MRW unreliable or unavailable |
    | `missing_ring` | No cells detected; ring added manually during cross-dating (e.g. wedging ring) |
    | `duplicate_ring` | Year appears in more than one image due to overlapping slides |
    | `exclude_dupl` | `TRUE` for the duplicate ring(s) not selected as the preferred source |
    | `exclude_issues` | Convenience flag combining incomplete and/or missing rings; controlled by `exclude_mode` |

    For the `exclude_mode` options (`"either"` or `"incomplete_only"`),
    see the main `rxs2tria` vignette.

If you need finer control — for example to add derived measures without
touching the flags, or to recompute flags after manual edits — the
underlying functions
[`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)
and
[`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)
can be called independently.

------------------------------------------------------------------------

## Checking a QWAdata object

``` r

check_QWAdata(QWA_data, meta = rxs_images)
```

[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
returns `TRUE` invisibly and issues warnings for any problems found:

- **Missing CWT** (warning): any image where `cwttan` is entirely `NA`.
- **Dating problems** (warning): undated years, duplicates, gaps, future
  years, or years beyond `outmost_year` (the last only if `meta` is
  supplied).
- **Structural errors** (error): both components `NULL`, or required
  identifier columns missing.

The `meta` argument is optional but recommended: without it, the
`outmost_year` check is skipped.

------------------------------------------------------------------------

## Persisting to disk

`QWAdata` is written as two separate CSV files — one for cells, one for
rings. Provide either a directory (files are named automatically) or
explicit paths:

``` r

# Using a directory — files named {dataset_name}_cells.csv.gz and
# {dataset_name}_rings.csv.gz
write_QWAdata(QWA_data, dir = "output/", dataset_name = "my_dataset")

# Or with explicit paths:
write_QWAdata(QWA_data,
              file_cells = "output/my_dataset_cells.csv.gz",
              file_rings = "output/my_dataset_rings.csv.gz")

# Read back:
QWA_data <- read_QWAdata(dir = "output/", dataset_name = "my_dataset")

# Read only rings (avoids loading a large cells file):
QWA_data_rings <- read_QWAdata(dir = "output/", dataset_name = "my_dataset",
                                components = "rings")
```

Compression defaults to `TRUE` for
[`write_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAdata.md)
(unlike
[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md),
where it defaults to `FALSE`).
[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
accepts both `.csv` and `.csv.gz`.
