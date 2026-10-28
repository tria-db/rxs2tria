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
QWA_data <- complete_QWAdata(QWA_data, meta = rxs_images)

# Step 3: check
check_QWAdata(QWA_data)
```

[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
reads the cells and rings tables from all ROXAS output files referenced
in a `QWAimages` object, cleans invalid values (negatives, error codes →
`NA`), and returns a minimal `QWAdata` with raw measurements only.

[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
then runs the full preprocessing pipeline (see [Completing a QWAdata
object](#completing-a-qwadata-object) below) and
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
checks the result.

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

**Derived measures** (`colType` `derived`, added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
`tca`, `rwd2`, `dcwt`, `cwtall_adj`, `cdrad`, `cdtan`, `cdratio`,
`sector100`.

**Estimate** (added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
`ew_lw` (earlywood / latewood classification based on Mork’s index;
depends on method choices, so always kept).

### `$rings`

One row per ring per image. Columns fall into four groups:

**Identifiers**: `woodpiece_label`, `slide_label`, `image_label`,
`year`.

**Measurements**: cell count (`cno`, recounted from `$cells`), mean ring
width (`mrw`), ring area (`ra`) and further ROXAS ring measures.
**Estimates** (added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
earlywood and latewood widths (`eww`, `lww`, based on Mork’s index).

**Quality flags** (added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)):
logical columns described in the next section.

**Quality-control inputs** (`colType` `qc_input`): further ring flags,
`affected_tissue` and `comment`, set in the flags app (see
[`vignette("flags-app")`](https://tria-db.github.io/rxs2tria/articles/flags-app.md)).
They are never added by
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md).

------------------------------------------------------------------------

## Completing a QWAdata object

``` r

QWA_data <- complete_QWAdata(QWA_data, meta = rxs_images)
```

[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
runs three operations in sequence and returns a new `QWAdata` object:

1.  **Year sequence completion** — extends `$rings` to cover every year
    in the cells data (filling gaps with `cno = 0` and `FALSE` for flag
    columns), and aborts on dating problems, including ring years after
    `outmost_year` from `meta` (+1 for an incomplete outermost ring).

2.  **Derived measures and estimates** — computes the derived and
    estimate columns listed above that are not yet present.
    Already-present columns are skipped, so re-running is safe.

3.  **Ring quality flags** — adds five logical columns to `$rings`:

    | Flag | Description |
    |----|----|
    | `incomplete_ring` | Ring at the inner or outer image border; MRW unreliable or unavailable |
    | `missing_ring` | No cells detected; ring added manually during crossdating (e.g. wedging ring) |
    | `duplicate_ring` | Year appears in more than one image due to overlapping slides |
    | `exclude_dupl` | `TRUE` for the duplicate ring(s) not selected as the preferred source |
    | `exclude_issues` | Rings to exclude from analyses; initialised from `incomplete_ring` |

    Missing rings are not excluded by default, as their zero width is a
    valid measurement. To use a different rule, set `exclude_issues`
    afterwards, e.g.
    `QWA_data$rings$exclude_issues <- QWA_data$rings$incomplete_ring | QWA_data$rings$missing_ring`.
    `NA` in `exclude_dupl` and `exclude_issues` counts as `FALSE`.
    [`extract_ring_measures()`](https://tria-db.github.io/rxs2tria/reference/extract_ring_measures.md)
    returns the identifier and measurement columns without the excluded
    rings, as input for further analyses.

If you need finer control, e.g. to add derived measures without adding
flags, call
[`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)
and
[`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)
separately. Both add only missing columns; to recompute a column, remove
it first.

------------------------------------------------------------------------

## Checking a QWAdata object

``` r

check_QWAdata(QWA_data)
```

[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
returns `TRUE` invisibly if no issue was found, `FALSE` otherwise. With
`strict = TRUE`, it aborts on the first issue instead of warning.

- **Errors**: invalid object, i.e. missing, mistyped or `NA`
  identifiers, or duplicate (image, year) in `$rings`.
- **Warnings**: values violating the schema (types, ranges, allowed
  values); duplicate cell keys (image, year, `xpix`, `ypix`); gaps and
  future years; images only in `$cells` or only in `$rings`, years with
  cells but without a rings row, and `cno` not matching the number of
  cells (re-run
  [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)).
- **Information**: missing optional columns, extra columns, and images
  where `cwttan` is entirely `NA`.

To check the data against the images (e.g. ring years after
`outmost_year`), use
`check_consistency(meta = rxs_images, data = QWA_data)`.

------------------------------------------------------------------------

## Persisting to disk

`QWAdata` is written as two separate CSV files — one for cells, one for
rings. Provide either a directory (files are named automatically) or
explicit paths:

``` r

# Using a directory — files named {dataset_name}_QWAcells.csv.gz and
# {dataset_name}_QWArings.csv.gz
write_QWAdata(QWA_data, dir = "output/", dataset_name = "my_dataset")

# Or with explicit paths:
write_QWAdata(QWA_data,
              file_cells = "output/my_dataset_QWAcells.csv.gz",
              file_rings = "output/my_dataset_QWArings.csv.gz")

# Read back:
QWA_data <- read_QWAdata(dir = "output/", dataset_name = "my_dataset")

# Read only rings (avoids loading a large cells file):
QWA_data_rings <- read_QWAdata(file_rings = "output/my_dataset_QWArings.csv.gz")
```

Compression defaults to `TRUE` for
[`write_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAdata.md)
(unlike
[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md),
where it defaults to `FALSE`).
[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
accepts both `.csv` and `.csv.gz`.

Empty values are written as empty fields. With `drop_empty = TRUE`
(default), optional `qc_input` columns that are empty in all rows (`NA`,
or `NA`/`FALSE` for flags) are not written;
[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
does not re-create them. With `keep_derived = FALSE`, derived columns
are not written; recompute them with
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md).
Estimates (`ew_lw`, `eww`, `lww`) are always written.
