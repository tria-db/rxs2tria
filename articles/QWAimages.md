# The QWAimages class

``` r

library(rxs2tria)
```

## Overview

A `QWAimages` object is a tibble subclass — one row per analyzed image —
that collects everything the rest of the package needs to know about
your raw ROXAS data: where the files live, how the images are organized
hierarchically, image EXIF properties, and the ROXAS (AI) settings used
during analysis. It is the required input for
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
and the `$images` component inside a `QWAmetadata` object.

The object carries a single attribute, `roxas_version` (`"roxas"` or
`"roxas_ai"`), which records which software produced the data and
determines the expected column set.

------------------------------------------------------------------------

## Building a QWAimages object

In normal use, you do not call
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
directly. Instead, you use
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md),
which combines the output of
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
(the hierarchical labels) with
[`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
(the per-image ROXAS settings and EXIF metadata). See the main
`rxs2tria` vignette for a step-by-step walkthrough.

``` r

files        <- get_roxas_files("path/to/ROXAS_data", roxas_version = "roxas")
df_structure <- extract_data_structure(files, pattern = "...")
df_settings  <- collect_settings_data(
  files_settings = df_structure$fname_settings,
  files_images   = df_structure$fname_image,
  roxas_version  = "roxas"
)

rxs_images <- build_QWAimages(df_structure, df_settings)
```

[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md)
calls
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
internally, which:

- infers or validates `roxas_version` from the `$software` column,
- coerces the data to a tibble and cleans column names,
- aligns the column set and types to the internal JSON schema for that
  software version (adding any missing required columns as `NA`,
  dropping unrecognised columns, and warning if coercions were made),
- runs a minimal structural check and aborts if the result is invalid.

If you already have a data frame with the right columns — for example
after reading from a non-standard source — you can pass it directly:

``` r

rxs_images <- QWAimages(my_df, roxas_version = "roxas")
```

------------------------------------------------------------------------

## What the object contains

``` r

rxs_images          # same as print(rxs_images): prints the tibble
summary(rxs_images) # compact overview
```

[`summary()`](https://rdrr.io/r/base/summary.html) reports the software
version, the count of images, slides, woodpieces, and trees, the sites
and species present, and the range of `outmost_year` values.

The columns fall into four groups:

**Hierarchical labels** — uniquely identify each image within the
sampling design and link images to the metadata tables in `QWAmetadata`:

| Column | Description |
|----|----|
| `site_label` | Site identifier |
| `tree_label` | Tree identifier (includes site) |
| `woodpiece_label` | Woodpiece identifier (includes tree) |
| `slide_label` | Slide identifier (includes woodpiece) |
| `image_label` | Image identifier (includes slide); used as the join key in `QWAdata` |
| `species_code` | Species code |

**File paths** — used by
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
and
[`update_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAdata.md)
to locate the ROXAS output files:

| Column           | Description                          |
|------------------|--------------------------------------|
| `fname_image`    | Path to the image file (ROXAS only)  |
| `fname_cells`    | Path to the cells output file        |
| `fname_rings`    | Path to the rings output file        |
| `fname_settings` | Path to the settings / metadata file |

**Image metadata** — properties extracted from the image EXIF data or
the ROXAS AI metadata file:

| Column | Description |
|----|----|
| `img_created_at` | Image capture timestamp |
| `img_width`, `img_height` | Image dimensions in pixels |
| `spatial_resolution` | Pixel size in µm; required for the incomplete-innermost-ring check in [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md) |
| `software` | Software that produced the data (`"roxas"` or `"roxas_ai"`); must match the `roxas_version` attribute |

**ROXAS settings** — parameters from the ROXAS settings or ROXAS AI
metadata file (e.g. magnification, threshold values). The exact columns
differ between `roxas_version` values.

**Dating** — `outmost_year` is the calendar year of the outermost
complete ring in the image, used to validate that ring years do not
exceed it.

Optional columns (those not always present in ROXAS output) are `NA`
until populated.
[`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md)
adds all optional columns at once, initialized to `NA`.

------------------------------------------------------------------------

## The `roxas_version` attribute and the `$software` column

`roxas_version` is stored both as the object attribute (the
authoritative source) and as the `$software` column (a per-row record).
The two must agree. When they do not, the package will warn:

``` r

# Warning raised by check_QWAimages() and during construction if attribute
# and $software column disagree:
#   ! roxas_version attribute ("roxas") does not match the software column ("roxas_ai").
#   i Check your data for mixed or incorrect software values.
```

This can happen if you manually edit the `$software` column or combine
objects from different software versions. The attribute always wins in
resolution; the `$software` column is used to re-derive `roxas_version`
when reading from file (since attributes are not persisted in CSV).

------------------------------------------------------------------------

## Checking a QWAimages object

``` r

check_QWAimages(rxs_images)
```

[`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md)
runs a series of validations and returns `TRUE` invisibly when done:

- **Schema compliance** (warning): checks that required columns are
  present and values are within expected ranges for the given
  `roxas_version`.
- **`roxas_version` consistency** (warning): checks that the attribute
  matches the `$software` column.
- **Missing optional columns** (warning): reports which optional columns
  are absent. Use
  [`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md)
  to add them.
- **Invalid structure** (error): aborts if required columns are missing
  or the object is otherwise structurally broken.

Warnings do not stop execution; they flag things you should inspect
before proceeding.

------------------------------------------------------------------------

## Adding optional columns

``` r

rxs_images_full <- complete_QWAimages(rxs_images)
```

Returns a new `QWAimages` object with every optional column added and
initialized to `NA`. Existing values are preserved. Use this before
passing the object to
[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
if you want to fill in optional fields manually.

------------------------------------------------------------------------

## Persisting to disk

``` r

write_QWAimages(rxs_images, "output/my_dataset_QWAimages.csv.gz")

rxs_images <- read_QWAimages("output/my_dataset_QWAimages.csv.gz")
```

[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md)
saves the tibble as a (optionally compressed) CSV. The `roxas_version`
attribute is **not** written to the file — it is re-derived from the
`$software` column when the file is read back by
[`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md).
This means the `$software` column must be intact and consistent for
round-tripping to work correctly.

The `compress` argument adds a `.gz` extension if not already present.
Alternatively, naming the file with a `.csv.gz` extension is sufficient
— `vroom` compresses automatically.
