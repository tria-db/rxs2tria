# Adding supplementary resources

``` r

library(rxs2tria)
```

## Overview

A TRIA submission must include at minimum three files: the `QWAmetadata`
`.json` and the two `QWAdata` `.csv(.gz)` files (cells and rings).
Beyond this minimum, you may optionally submit **supplementary
resources**, e.g. the original or annotated images, the raw ROXAS /
ROXAS AI output files, reference series, and so on.

[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
scans a directory (or an already-zipped archive) and returns a
**manifest**: a table with one row per file, recording what each file
is, where it sits in the data hierarchy, and whether it is actually
ready to be submitted. Building it does **not** move, rename, or
compress any files—they stay where they are on your machine and are
referenced by their relative path.

As described in
[`vignette("submission")`](https://tria-db.github.io/rxs2tria/articles/submission.md),
a submission’s supplementary files are expected to live in a **single
directory** (which you zip up before uploading).
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
scans it recursively, so you may use your preferred nested file
structure within that directory. It also accepts a path to an
already-zipped (non-nested) archive directly. The files in the scanned
directory or archive are automatically assigned a resource type and
linked to the appropriate image or woodpiece, etc., where appropriate.
However, in some cases, you may need to correct or edit the inferred
values (see [Manual editing](#manual-editing)) or the underlying files
themselves. Run
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
to re-check the edits and re-sync the file list in the table (see
[Reconciling and verifying the
manifest](#reconciling-and-verifying-the-manifest)). Once finalised,
write the manifest to a CSV and submit it alongside the supplementary
resources zip.

------------------------------------------------------------------------

## The resources table

In the data frame returned by
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
/
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md),
each row has the following columns:

| Column | Meaning |
|----|----|
| `resource_name` | Path of the file relative to the directory / archive scanned. |
| `resource_type` | The kind of file, inferred from its name (see [Resource types](#resource-types)). |
| `linked_level` | Level of the data hierarchy the file pertains to: `"dataset"`, `"site"`, …, or `"analysis"`. |
| `linked_label` | Which entity at that level the file belongs to (e.g. a specific `image_label`). Auto-filled where possible, otherwise `NA` (see [Linked labels](#linked-labels)). |
| `description` | Free-text description of the file, normally `NA`; required for `"other"` resources to be included (see [Manual editing](#manual-editing)). |
| `fname_resource` | Absolute path to the file on your machine, or `NA` if scanning a zip archive directly (always removed before upload to TRIA). |
| `status` | `"ok"`, `"review"`, or `"ignore"` (see [Readiness status](#readiness-status)). |
| `note` | Explanation for `status` (e.g., why a file needs review). |

Every file found in the directory or archive gets a row, thus the table
is a complete, self-explanatory record of what you’re about to submit.
`resource_name` includes the subdirectory path
(e.g. `"tree1/slide2/IMG1_Output_Cells.txt"`) rather than just the
basename, so two files with the same name in different subfolders stay
distinguishable.

------------------------------------------------------------------------

## Basic workflow

Point
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
at the directory holding your supplementary files (subdirectories are
included). `rxs_images` (a `QWAimages` object) is required to
auto-populate `linked_label` (see [Linked labels](#linked-labels)):

``` r

QWA_meta <- read_QWAmetadata("output_data/my_dataset_QWAmetadata.json")
suppl_res <- compile_resources("path/to/supplementary_files", rxs_images = QWA_meta$images)
```

Inspect the result. Warning messages indicate whether anything needs
review. Complete any resource types or labels that require manual edits
(see [Manual editing](#manual-editing)), re-compile the manifest if
necessary and check the finalised table against the submission-ready
supplementary directory or archive (see [Reconciling and verifying the
manifest](#reconciling-and-verifying-the-manifest)). Then write the
prepared manifest to a CSV to submit alongside the resources zip:

``` r

suppl_res

vroom::vroom_write(suppl_res, "output_data/supplementary_files.csv")
```

### Manual editing

Inference is name-based, so an unconventional file name may be typed as
`"other"` or mis-typed. Edit the table directly to fix it (see also
[Readiness status](#readiness-status)):

``` r

suppl_res[suppl_res$resource_name == "tree1/slide2/odd_name.tif",
  c("resource_type", "linked_level", "linked_label")] <-
    c("image_original", "image", "siteA_PISY_1_2_1")
```

Set `resource_type` to `"junk"` for any files you want to be ignored,
and fill in the `description` field for any `"other"` resources you want
to be included.

``` r

suppl_res$description[suppl_res$resource_name == "siteA_photo.png"] <-
  "Image from the site location"
```

Instead of or in addition to editing the data frame itself, you may want
to make changes in the input directory or archive under `path` directly
(adding, removing or renaming files). Then re-run
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
to create a fresh manifest, or run
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
with your previous manifest as input to keep and re-check any manual
edits you have already made in the data frame while re-syncing it with
the file list under `path`.

### Reconciling and verifying the manifest

Three functions cover the full lifecycle:

- `compile_resources(path, rxs_images)`: always starts fresh.
- `recompile_resources(suppl_res, path, rxs_images, add_new_files = FALSE)`:
  reconciles a previous manifest against the current contents of `path`,
  so manual edits are kept and re-checked.
- `check_supplementary(suppl_res, path, rxs_images)`: a pass/fail check
  confirming a finalised manifest (i.e. including the `status` column)
  accurately describes `path` and is ready for submission.

For both
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
and
[`check_supplementary()`](https://tria-db.github.io/rxs2tria/reference/check_supplementary.md),
`suppl_res` is treated as the source of truth wherever it overlaps with
what’s actually found in `path`:

- Any file both in `suppl_res` and found in `path` keeps its
  `resource_type`/`linked_level`/`linked_label`/`description` from
  `suppl_res` rather than having them re-inferred, so manual edits
  survive.
- A file found in `path` but not listed in `suppl_res` is, by default,
  assumed to be intentionally excluded and thus also left out of the
  returned table. When re-compiling, you can pass `add_new_files = TRUE`
  to append new files instead, freshly typed, e.g. after deliberately
  adding files to `path` and wanting them picked up.
- A file listed in `suppl_res` but no longer found in `path` is  
  dropped (with a message) if `status` was `"review"`, `"ignore"` or not
  set (e.g. in a hand-built manifest)—deleting it from `path` resolves
  the status issue. However, if it had `status = "ok"` in the manifest,
  an error is raised—a submission-ready file has disappeared and either
  the table or `path` needs fixing before proceeding.

``` r

suppl_res <- vroom::vroom("output_data/supplementary_files.csv",
                          show_col_types = FALSE)
# after manual edits and/or changing the supplementary dir contents:
suppl_res <- recompile_resources(suppl_res, "path/to/supplementary_files",
                                 rxs_images = QWA_meta$images,
                                 add_new_files = TRUE)
# to re-check a created supplementary zip against the finalised manifest
check_supplementary(suppl_res, "supplementary_data.zip", rxs_images = QWA_meta$images)
vroom::vroom_write(suppl_res, "output_data/supplementary_files.csv")
```

The resources manifest CSV submitted alongside the supplementary zip
will be treated as the single point of truth. Thus any files in the zip
but not listed in the manifest (or listed as `resource_type = "junk"`)
will **not** be included in the TRIA upload. To be explicit about your
choices and to limit the size of the supplementary data submission, we
recommend removing any files you do not want to include from the
supplementary directory before zipping, followed by re-running
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
to update the manifest.

------------------------------------------------------------------------

## Resource types

`resource_type` is inferred from each file name by matching it against a
table of naming patterns bundled with the package
(`system.file("extdata", "resource_types.csv", package = "rxs2tria")`).
For example `*_Output_Cells.txt` becomes `"roxas_output_cells"`,
`*.metadata.json` becomes `"rai_metadata"`, etc.

| resource_type | linked_level | example_names | identifier | source |
|:---|:---|:---|:---|:---|
| QWAdata_cells | dataset | DSName_QWAcells.csv.gz | QWAcells.csv(.gz)?\$ | rxs2tria |
| QWAdata_rings | dataset | DSName_QWArings.csv | QWArings.csv(.gz)?\$ | rxs2tria |
| QWAmetadata | dataset | DSName_QWAmetadata.json | QWAmetadata.json\$ | rxs2tria |
| QWAprofile | dataset | DSName_QWAprofile.csv | QWAprofile.csv(.gz)?\$ | rxs2tria |
| reference_series | dataset | SITE_SPECIES.rwl | .rwl\$ | processing |
| image_panorama | woodpiece | WPNAME_Panorama.png; WPNAME_Panorama.xcf | Panorama.(png\|xcf\|tif\|tiff)\$ | processing |
| image_original | image | IMGNAME.jpg; IMGNAME.scan.jpg; IMGNAME.scan.jpeg; IMGNAME.rxs.jpg; IMGNAME.png | (.(scan\|rxs))?.(jpg\|jpeg\|png)\$ | processing |
| image_preview | slide | SLIDENAME_Preview.jpg | Preview.(jpg\|jpeg)\$ | processing |
| roxas_full_output_summary | dataset | \_ROXAS_Output_Summary\_\_DSNAME.xlsx | ROXAS_Output_Summary.\*.xlsx\$ | roxas |
| image_refseries | analysis | IMGNAME_ReferenceSeries.jpg; IMGNAME_ReferenceSeries.gif; IMGNAME_ReferenceSeries_Original.jpg; IMGNAME_ReferenceSeriesLong.jpg | \_ReferenceSeries(Long\|\_Original)?.(jpg\|jpeg\|gif)\$ | roxas |
| roxas_image_annotated | analysis | IMGNAME_annotated.jpg | \_annotated.(jpg\|jpeg\|png)\$ | roxas |
| roxas_image_annotated_cells | analysis | IMGNAME_annotated_cells.jpg | \_annotated_cells.(jpg\|jpeg)\$ | roxas |
| roxas_image_annotated_twin | analysis | IMGNAME_annotated_twin.jpg | \_annotated_twin.(jpg\|jpeg)\$ | roxas |
| roxas_output_cells | analysis | IMGNAME_Output_Cells.txt | \_Output_Cells.txt\$ | roxas |
| roxas_output_rings | analysis | IMGNAME_Output_Rings.txt | \_Output_Rings.txt\$ | roxas |
| roxas_output_xlsx | analysis | IMGNAME_Output.xlsx | \_Output.xlsx\$ | roxas |
| roxas_output_summary | analysis | IMGNAME_Output_Summary.txt | \_Output_Summary.txt\$ | roxas |
| roxas_settings | analysis | IMGNAME_ROXAS_Settings.txt | \_ROXAS_Settings.txt\$ | roxas |
| roxas_shapefile_vessels | analysis | IMGNAME_Vessels.scl | \_Vessels.scl\$ | roxas |
| junk | NA | IMGNAME_Vessels_bu.scl | \_Vessels_bu.scl\$ | roxas |
| roxas_shapefile_ringtraces | analysis | IMGNAME_RingTraces.txt | \_RingTraces.txt\$ | roxas |
| junk | NA | IMGNAME_RingTraces_bu.txt | \_RingTraces_bu.txt\$ | roxas |
| roxas_cal | analysis | IMGNAME.cal | .cal\$ | roxas |
| roxas_AOI | analysis | IMGNAME_AOI.out; IMGNAME_AOI_40.out; IMGNAME_AOI_50_25.out; etc | *AOI(*\[0-9\]+)\*.out\$ | roxas |
| roxas_AOE | analysis | IMGNAME_AOE.out | \_AOE.out\$ | roxas |
| roxas_AOEClass | analysis | IMGNAME_AOEClass.txt | \_AOEClass.txt\$ | roxas |
| roxas_CWT | analysis | IMGNAME_CellWallThickness.out | \_CellWallThickness.out\$ | roxas |
| roxas_ringinclination | analysis | IMGNAME_RingInclination.out | \_RingInclination.out\$ | roxas |
| roxas_junkobjects | analysis | IMGNAME_JunkObjects.scl; IMGNAME_JunkObjects2.scl; etc | \_JunkObjects\[0-9\]\*.scl\$ | roxas |
| roxas_proj | analysis | IMGNAME_proj.rpf | \_proj.rpf\$ | roxas |
| rai_metadata | analysis | IMGNAME.metadata.json | .metadata.json\$ | roxas_ai |
| rai_cells_table | analysis | IMGNAME.cells_table.csv; IMGNAME.cells_table.txt | .cells_table.(csv\|txt)\$ | roxas_ai |
| rai_rings_table | analysis | IMGNAME.rings_table.csv; IMGNAME.rings_table.txt | .rings_table.(csv\|txt)\$ | roxas_ai |
| rai_image_cells | analysis | IMGNAME.cells.png | .cells.png\$ | roxas_ai |
| rai_image_rings | analysis | IMGNAME.rings.tiff | .rings.(tif\|tiff)\$ | roxas_ai |
| rai_image_annotated | analysis | IMGNAME_annotated.jpg | \_annotated.(jpg\|jpeg\|png)\$ | roxas_ai |
| junk | NA | Thumbs.db | ^Thumbs.db\$ | system |
| junk | NA | IMGNAME_annotated.cal | \_annotated.cal\$ | roxas |
| junk | NA | SLIDENAME_Preview.cal | Preview.cal\$ | roxas |
| junk | NA | IMGNAME_ReferenceSeries.cal | \_ReferenceSeries(Long\|\_Original)?.cal\$ | roxas |
| junk | NA | IMGNAME_TRACHEIDMASK.tif | \_TRACHEIDMASK.(tif\|tiff)\$ | roxas |
| junk | NA | FILENAME~RF6c4558.TMP | ~RF\[\[:alnum:\]\]+.TMP\$ | system |
| junk | NA | ~\$IMGNAME_Output.xlsx \|^~\\ \|system \| \|junk \|NA \|desktop.ini \|^desktop\\ini\$ | system |  |
| junk | NA | .DS_Store | .DS_Store\$ | system |
| junk | NA | .\_IMGNAME.jpg | ^.\_ | system |

Patterns are tested most specific first and the first match wins. The
`roxas_version` attribute of the input `rxs_images` is used to subset
the list of search patterns and to disambiguate patterns shared between
classic ROXAS and ROXAS AI. Known backup and junk files (ROXAS `_bu`
backups, `Thumbs.db`, Office lock files, hidden files, …) are typed as
`"junk"` and will be ignored automatically. Thus you can also set the
resource type to `"junk"` for any file you want to be excluded from the
TRIA upload. Any rxs2tria-generated files (`QWAmetadata.json`, the
`QWAcells`/`QWArings`/`QWAprofile` `.csv(.gz)` files) under the input
`path` are also detected.

**Anything else** is typed as `"other"` (with `linked_level = NA` and
`status = "review"`). If the file should be included in the TRIA upload,
provide a brief `description` explaining what the file is and why it is
relevant.

------------------------------------------------------------------------

## Linked labels

Every resource type has a default `linked_level` describing the level of
the data hierarchy the file pertains to: `"dataset"` (applies to the
whole submission, e.g. a reference chronology), `"site"`, … , or
`"analysis"` (per-analysed-image ROXAS (AI) files such as shapefiles or
annotated images). `linked_label` identifies *which* entity at that
level the file belongs to (e.g. a specific `image_label`). Where
possible, the label is filled automatically from the data structure
defined in `rxs_images`—the `QWAimages` object or `$images` component of
the `QWAmetadata` object that you pass to
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
/
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md).

For `image`/`analysis` files, matching works by testing whether a
resource’s *basename* (`resource_name` with any directory stripped)
**starts with** an image’s base name (`rxs_images$org_img_name` without
any extensions); the longest match wins, so labels that share a prefix
are not confused (e.g. `S22_L1` vs `S22_L10`).

For `slide`/`woodpiece` files (e.g. `SLIDENAME_Preview.jpg`,
`WPNAME_Panorama.tif`), there is no “original” slide/woodpiece name
stored anywhere to match against directly—only the constructed
`slide_label`/`woodpiece_label`. Instead, the original identifier is
recovered from the images that share a `slide_label` (or
`woodpiece_label`): since sibling images are almost always named with a
common prefix followed by an image-specific suffix, the longest common
prefix of their `org_img_name` values recovers that original identifier,
whatever the naming convention. A slide/woodpiece with only a single
image has no sibling to compare against, so its trailing token (the part
after the last separator, e.g. `_1` in `SITEA-PISY_01_2_1`) is stripped
instead.

Files that do not match anything are left as `NA`, and resource types
that normally need a `linked_label` are flagged `status = "review"`
until one is filled in (see [Readiness status](#readiness-status)).

``` r

suppl_res$linked_label[suppl_res$resource_name == "SITEA_PISY.rwl"] <- "SITEA_PISY"
suppl_res$linked_level[suppl_res$resource_name == "SITEA_PISY.rwl"] <- "site" # for a reference series at a specific site
```

Always review the auto-filled labels, too: the prefix-based matching is
a heuristic and can occasionally miss or mismatch unconventional file
names.

------------------------------------------------------------------------

## Readiness status

Every row is assigned a `status`, computed automatically by
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)/[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md):

- **`"ok"`**: recognised, passes every check, ready to submit as-is. The
  file is of a recognised type and a linked label could be inferred as
  needed. An rxs2tria-generated file found among your supplementary
  files (`QWAmetadata.json`, the `QWAcells`/`QWArings`/ `QWAprofile`
  `.csv(.gz)` files) also gets `"ok"` here. You may include it in the
  zip or submit it individually, either is fine.
- **`"review"`**: needs your attention before this dataset can be zipped
  for submission. `note` says why: an unrecognised (`"other"`) file
  without a `description` justifying its inclusion, a missing
  `linked_label` where one is expected, more than one file of a type
  expected to occur once per level, a resource type not expected for
  this ROXAS version, or a `linked_label` that doesn’t match any known
  label.
- **`"ignore"`**: a junk file (ROXAS `_bu` backups, stray
  calibration/scan files, `Thumbs.db`, `desktop.ini`, `.DS_Store`,
  Office lock files, hidden files, …). Never part of what gets uploaded
  as supplementary resources to TRIA. Any file where you manually set
  the `resource_type` to `"junk"` gets the same treatment. However, you
  may want to remove these files from the directory yourself to decrease
  the submission file size.

The console output tells you where things stand: once no row needs
review, you’ll see a confirmation that the dataset is ready to zip;
otherwise the files that need attention are listed together with their
`note`. A file typed `"other"` is treated as ready (`"ok"`) as soon as
you give it a `description` explaining why it belongs in the submission
as-is—you don’t need to also assign it a “real” `resource_type` unless
one actually applies.

``` r

suppl_res$resource_type[suppl_res$resource_name == "tree1/slide2/odd_name.tif"] <- "image_original"
suppl_res$linked_level[suppl_res$resource_name == "tree1/slide2/odd_name.tif"] <- "image"

# re-run the checks after manually editing the table:
suppl_res <- recompile_resources(suppl_res, "path/to/submission_files", QWA_meta$images)
```

------------------------------------------------------------------------
