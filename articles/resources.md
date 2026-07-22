# Adding supplementary resources

``` r

library(rxs2tria)
```

## Overview

A TRIA submission must include at minimum three files: the `QWAmetadata`
`.json` and the two `QWAdata` `.csv` files (cells and rings). Beyond
this minimum, you may optionally submit **supplementary resources** —
the original and annotated images, the raw ROXAS / ROXAS AI output
files, reference series, and so on — so that others can trace,
reproduce, or build on your analysis.

Supplementary resources are described in the `$resources` component of
the `QWAmetadata` object. This component is a table (a
\[tibble\]\[tibble::tibble\]) with **one row per file**. It is a
*manifest*: it records what each file is, where it sits in the data
hierarchy, and a checksum and size for integrity checking. Building it
does **not** move, rename, or compress any files — they stay where they
are on your machine and are referenced by path.

You build the table with two functions:

- [`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
  — the convenient entry point; scans a directory and updates the
  `$resources` component of a `QWAmetadata` object in place.
- [`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md)
  — the underlying worker; returns a plain resources table and is useful
  when you want to inspect or manipulate it directly before attaching
  it.

------------------------------------------------------------------------

## The resources table

Each row has the following columns:

| Column | Meaning |
|----|----|
| `resource_name` | Base file name (no path). |
| `resource_type` | The kind of file, inferred from its name (see below). |
| `linked_level` | Level of the data hierarchy the file pertains to: `"dataset"`, `"woodpiece"`, `"slide"`, `"image"`, or `"analysis"`. |
| `linked_label` | Which entity at that level the file belongs to (e.g. a specific `image_label`). Auto-filled where possible, otherwise `NA`. |
| `fname_resource` | Absolute path to the file on your machine. |
| `checksum` | MD5 checksum of the file contents. |
| `size_bytes` | File size in bytes. |

`checksum` and `size_bytes` are computed automatically. They let the
TRIA ingestion process verify that the files you upload match this
manifest exactly and detect anything missing, extra, or changed — you do
not need to fill or check them yourself.

You do not need to think about *how* these files are ultimately stored
in the TRIA archive (e.g. whether they are grouped into `.zip`
archives). That packaging is handled downstream from this manifest; your
task is only to produce an accurate listing.

------------------------------------------------------------------------

## Basic workflow

Start from a `QWAmetadata` object — either one you built earlier in the
preparation workflow, or one read back from disk:

``` r

QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
```

Point
[`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
at a directory of files. Set `recursive = TRUE` to descend into
subdirectories (raw ROXAS output is often nested per tree or slide):

``` r

QWA_meta <- add_resources(QWA_meta, path = "raw_roxas_files", recursive = TRUE)
```

Call it again for each additional directory; new files are appended to
the existing table:

``` r

QWA_meta <- add_resources(QWA_meta, path = "annotated_images")
QWA_meta <- add_resources(QWA_meta, path = "reference_series")
```

Inspect the result, complete any labels that need attention (see
[Linking resources to the data
hierarchy](#linking-resources-to-the-data-hierarchy)), and write the
object back to disk:

``` r

QWA_meta$resources

write_QWAmetadata(QWA_meta, "output_data/my_dataset_QWAmetadata.json")
```

### Working with the table directly

If you prefer to assemble and edit the table before attaching it, use
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md).
It takes the same arguments and can chain across directories via
`append_to`:

``` r

res <- collect_resources("raw_roxas_files", recursive = TRUE)
res <- collect_resources("annotated_images", append_to = res)

# edit `res` as needed, then attach it:
QWA_meta$resources <- res
```

------------------------------------------------------------------------

## Resource types

`resource_type` is inferred from each file name by matching it against
the standard ROXAS, ROXAS AI, and QWA naming conventions. Patterns are
tested **most-specific first**. The recognised types are:

**QWA pipeline outputs** (`linked_level = "dataset"`)

| Type                        | File name pattern                           |
|-----------------------------|---------------------------------------------|
| `QWAdata_cells`             | `*_QWAdata_cells*.csv(.gz)`                 |
| `QWAdata_rings`             | `*_QWAdata_rings*.csv(.gz)`                 |
| `QWAprofile`                | `*_QWAprofile*.csv(.gz)`                    |
| `QWAmetadata`               | `*.json(.gz)` (excluding `*.metadata.json`) |
| `roxas_full_output_summary` | `*_Output_Summary_ROXAS.xlsx`               |
| `reference_series`          | `*.rwl`                                     |

**Classic ROXAS per-image output** (`linked_level = "analysis"`)

| Type                         | File name pattern         |
|------------------------------|---------------------------|
| `roxas_output_cells`         | `*_Output_Cells.txt`      |
| `roxas_output_rings`         | `*_Output_Rings.txt`      |
| `roxas_output_summary`       | `*_Output_Summary.txt`    |
| `roxas_output_xlsx`          | `*_Output*.xlsx`          |
| `roxas_settings`             | `*_ROXAS_Settings.txt`    |
| `roxas_shapefile_ringtraces` | `*_RingTraces.txt`        |
| `roxas_shapefile_vessels`    | `*_Vessels.scl`           |
| `roxas_junkobjects`          | `*_JunkObjects.scl`       |
| `roxas_AOI`                  | `*_AOI.out`               |
| `roxas_AOE`                  | `*_AOE.out`               |
| `roxas_AOEClass`             | `*_AOEClass.txt`          |
| `roxas_CWT`                  | `*_CellWallThickness.out` |
| `roxas_cal`                  | `*.cal`                   |
| `roxas_proj`                 | `*_proj.rpf`              |

**ROXAS AI per-image output** (`linked_level = "analysis"`)

| Type              | File name pattern   |
|-------------------|---------------------|
| `rai_metadata`    | `*.metadata.json`   |
| `rai_cells_table` | `*.cells_table.csv` |
| `rai_rings_table` | `*.rings_table.csv` |
| `rai_image_cells` | `*.cells.png`       |
| `rai_image_rings` | `*.rings.tif(f)`    |

**Images**

| Type | File name pattern | `linked_level` |
|----|----|----|
| `roxas_image_annotated_cells` | `*_annotated_cells.jpg/jpeg` | `analysis` |
| `roxas_image_annotated_twin` | `*_annotated_twin.jpg/jpeg` | `analysis` |
| `roxas_image_annotated` | `*_annotated.jpg/jpeg` | `analysis` |
| `image_refseries` | `*_ReferenceSeries.jpg/jpeg/gif` | `image` |
| `image_preview` | `*_Preview*.jpg/jpeg` | `slide` |
| `image_panorama` | `*panorama*.jpg/jpeg/tif/tiff` | `woodpiece` |
| `image_original` | any other `*.jpg/jpeg/tif/tiff` | `image` |

**Anything else** is typed as `"other"` (with `linked_level = NA`). By
default `"other"` files are dropped from the table; pass
`include_unmatched = TRUE` to keep them so you can assign a type
manually.

### Excluded files

Some files are always skipped, regardless of `include_unmatched`,
because they are backups or system artefacts: ROXAS `_bu` backups
(`*_Vessels_bu.scl`, `*_Ringtraces_bu.txt`), annotated calibration files
(`*_annotated.cal`), `Thumbs.db`, and Office lock files (`~$*`).

### Correcting an inferred type

Inference is name-based, so an unconventional file name may be typed as
`"other"` or mis-typed. Edit the table directly to fix it:

``` r

res$resource_type[res$resource_name == "odd_name.tif"] <- "image_original"
res$linked_level[res$resource_name == "odd_name.tif"] <- "image"
```

------------------------------------------------------------------------

## Linking resources to the data hierarchy

`linked_level` says *what kind* of entity a file belongs to;
`linked_label` says *which one*. For per-image files (`linked_level` of
`"image"` or `"analysis"`), the label can be filled automatically from a
**data structure** — either the table returned by
\[extract_data_structure()\] or the `$images` component of your
`QWAmetadata` object.

[`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
does this for you: it passes `x$images` as the data structure, so
per-image resources are matched to their `image_label` without any extra
step. The console reports how many were matched, e.g.
`Matched 42/45 resources to an image label`.

Matching works by testing whether a resource’s file name **starts with**
an image’s base name; the longest match wins, so labels that share a
prefix are not confused (e.g. `S22_L1` vs `S22_L10`). Files that do not
match any image are left as `NA`.

With
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md),
supply the structure explicitly:

``` r

res <- collect_resources("raw_roxas_files",
                         df_structure = QWA_meta$images,
                         recursive = TRUE)
```

Labels for `dataset`-, `woodpiece`-, and `slide`-level resources are not
inferred and remain `NA`. Fill in the ones that matter — for example,
tie a woodpiece panorama image to its woodpiece:

``` r

res$linked_label[res$resource_name == "SITEA_PISY_01_panorama.tif"] <-
  "SITEA_PISY_01"
```

A `linked_label` of `NA` is acceptable — it simply means the file is not
tied to a specific entity below its level (a dataset-wide reference
chronology is a natural example).

------------------------------------------------------------------------

## Adding an optional description

The resources schema also allows a free-text `description` per file,
useful when a resource needs context that its type does not convey. It
is not created by
[`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md);
add it yourself when helpful:

``` r

res$description[res$resource_type == "reference_series"] <-
  "Master chronology used for crossdating, EPS > 0.85."
```

------------------------------------------------------------------------

## Where this fits in the workflow

Compiling resources is the optional final step of the preparation
workflow (see
[`vignette("workflow")`](https://tria-db.github.io/rxs2tria/articles/workflow.md),
Step 7). Once `$resources` is populated and the `QWAmetadata` object is
written to disk, the manifest travels with the submission and the listed
files can be uploaded alongside the required `QWAmetadata` and `QWAdata`
files.
