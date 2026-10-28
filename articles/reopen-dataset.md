# Re-opening a TRIA dataset

``` r

library(rxs2tria)
```

## Overview

The main purpose of `rxs2tria` is to turn raw ROXAS / ROXAS AI output
into the files for a TRIA submission (see
[`vignette("workflow")`](https://tria-db.github.io/rxs2tria/articles/workflow.md)).
The `read_*` functions also read TRIA dataset files, so a dataset
downloaded from TRIA — or your own submission files — can be re-opened
and worked with using the package.

A TRIA dataset consists of at minimum three files:

- the `QWAmetadata` `.json`,
- the `QWAdata` cells `.csv`,
- the `QWAdata` rings `.csv`.

In a downloaded TRIA dataset folder these are in `data/`
(`<ds_label>_QWAmetadata.json`, `<ds_label>_QWAcells.csv.gz`,
`<ds_label>_QWArings.csv.gz`); supplementary files are in
`supplementary/`.

These three are all you need to re-open a dataset. The optional
supplementary files (original images, raw ROXAS output, etc.) are
**not** required for anything shown here — download them only if you
specifically need the raw material behind the measurements.

------------------------------------------------------------------------

## Reading the individual files

Each file is read by its corresponding `read_*` function.
[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
takes the path to the `.json` file directly:

``` r

QWA_meta <- read_QWAmetadata("path/to/my_dataset_QWAmetadata.json")
QWA_meta$images
```

[`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
recognises TRIA dataset files (`is_submission(QWA_meta)` is `FALSE`).
Their `$datafiles` component lists the cells and rings files with their
MD5 `checksum`. TRIA dataset files are not edited:
[`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md),
[`update_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAmetadata.md)
and
[`combine_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAmetadata.md)
abort.

[`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
reads the cells and rings CSVs. Point it at the directory holding the
files (for a TRIA dataset, the `data/` folder; the search is not
recursive) and it locates both by name; if the folder holds files from
more than one dataset, pass `dataset_name` to disambiguate:

``` r

QWA_data <- read_QWAdata(dir = "path/to/tria.0000/data")
# or with explicit paths:
QWA_data <- read_QWAdata(file_cells = "path/to/my_dataset_QWAcells.csv.gz",
                         file_rings = "path/to/my_dataset_QWArings.csv.gz")
```

TRIA dataset files do not include the derived cell measures; recompute
them with:

``` r

QWA_data <- complete_QWAdata(QWA_data, QWA_meta$images)
```

If the supplementary files include a profile (`QWAprofile` CSV),
[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md)
reads it back into a `QWAprofile` object:

``` r

prf <- read_QWAprofile("path/to/my_dataset_QWAprofile_sector.csv.gz")
```

------------------------------------------------------------------------

## Recomputing a QWAprofile

Because the cells and rings tables carry the columns the profile
calculation needs (`rraddistr` in cells, `mrw` in rings for band
profiles), you can compute a `QWAprofile` directly from `QWA_data` —
even if no profile was included in the download. To select derived cell
parameters, recompute them first (see above):

``` r

prf_sector <- calculate_sector_profiles(
  QWA_data,
  n_sectors       = 5,
  sel_cell_params = c("la", "cwttan"),
  quant_probs     = c(0.1, 0.5, 0.9)
)
```

See
[`vignette("QWAprofile")`](https://tria-db.github.io/rxs2tria/articles/QWAprofile.md)
for the difference between sector and band profiles and the full set of
options.

------------------------------------------------------------------------

## Reviewing ring flags in the flags app

The ring flags shipped in the downloaded rings file (`incomplete_ring`,
`missing_ring`, `exclude_issues`, …) can be inspected and edited in the
flags app. In TRIA dataset files, missing flag values are stored as
`FALSE`, and optional quality flags that are `FALSE` in all rows are not
included. Launch the app with:

``` r

launch_flags_app()
```

In the app, choose **“Load data from csv files”** and provide:

- **QWA profile data** (optional) — a profile CSV, e.g. one you wrote
  with
  [`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
  after computing it as above.
- **QWA rings data** — the rings CSV (`.csv` or `.csv.gz`) of the
  dataset.
- **Images metadata** — here you can upload the **QWAmetadata `.json`**
  directly: the app extracts its `$images` component automatically. (A
  standalone `QWAimages` `.csv` is also accepted.)

Alternatively, choose **“Load data from R environment”** and refer to
objects you have already read in, for example `prf_sector`,
`QWA_data$rings`, and `QWA_meta$images`.

Opening images from the app is not available for TRIA dataset files, as
they carry no local image paths.

------------------------------------------------------------------------

## What to keep in mind

- **Reading re-validates.**
  [`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
  checks the required and measurement columns, re-derives complete year
  sequences, and verifies dating. Missing measurement columns give
  warnings — these are informative and do not stop you from working with
  the data that is present.
- **Supplementary files are a separate path.** Re-opening (this
  vignette) needs only the three core files. Re-running the raw
  preprocessing pipeline
  ([`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md),
  [`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md),
  …) is a different workflow that operates on the raw ROXAS files, which
  are only available if they were submitted as supplementary files (see
  [`vignette("supplementary")`](https://tria-db.github.io/rxs2tria/articles/supplementary.md)).
