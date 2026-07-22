# Re-opening a TRIA dataset

``` r

library(rxs2tria)
```

## Overview

The main purpose of `rxs2tria` is to turn raw ROXAS / ROXAS AI output
into a submission-ready TRIA dataset (see
[`vignette("workflow")`](https://tria-db.github.io/rxs2tria/articles/workflow.md)).
The same `read_*` functions that write a dataset can also **read one
back**, so a dataset downloaded from the TRIA database — or one you
prepared earlier — can be re-opened and worked with using the package.

A TRIA dataset consists of at minimum three files:

- the `QWAmetadata` `.json`,
- the `QWAdata` cells `.csv`,
- the `QWAdata` rings `.csv`.

These three are all you need to re-open a dataset. The optional
supplementary files (original images, raw ROXAS output, etc.) are
**not** required for anything shown here — download them only if you
specifically need the raw material behind the measurements.

------------------------------------------------------------------------

## Reading the individual files

Each file is read by its corresponding `read_*` function.
\[read_QWAmetadata()\] takes the path to the `.json` file directly:

``` r

QWA_meta <- read_QWAmetadata("path/to/my_dataset_QWAmetadata.json")
QWA_meta$images
```

\[read_QWAdata()\] reads the cells and rings CSVs. Point it at the
directory holding the downloaded files and it locates both by name; if
the folder holds files from more than one dataset, pass `dataset_name`
to disambiguate:

``` r

QWA_data <- read_QWAdata(dir = "path/to/downloaded_dataset")
# or with explicit paths:
QWA_data <- read_QWAdata(file_cells = "path/to/my_dataset_cells.csv.gz",
                         file_rings = "path/to/my_dataset_rings.csv.gz")
```

If a profile file was included in the download, \[read_QWAprofile()\]
reads it back into a \[QWAprofile\]\[QWAprofile()\] object:

``` r

prf <- read_QWAprofile("path/to/my_dataset_QWAprofile_sector.csv.gz")
```

------------------------------------------------------------------------

## Recomputing a QWAprofile

Because the cells and rings tables carry the columns the profile
calculation needs (`rraddistr` in cells; `mrw` and `eww` in rings, plus
the cell parameters you select), you can compute a
\[QWAprofile\]\[QWAprofile()\] directly from `QWA_data` — even if no
profile was included in the download:

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
flags app. Launch it with:

``` r

launch_flags_app()
```

In the app, choose **“Load data from csv files”** and provide:

- **QWA profile data** — a profile CSV, e.g. one you wrote with
  [`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
  after computing it as above.
- **QWA rings data** — the rings CSV of the dataset.
- **Images metadata** — here you can upload the **QWAmetadata `.json`**
  directly: the app extracts its `$images` component automatically. (A
  standalone `QWAimages` `.csv` is also accepted.)

Alternatively, choose **“Load data from R environment”** and refer to
objects you have already read in, for example `prf_sector`,
`QWA_data$rings`, and `QWA_meta$images`.

------------------------------------------------------------------------

## What to keep in mind

- **Reading re-validates.**
  [`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
  checks the data against the package schema, re-derives complete year
  sequences, and verifies dating. If a file is missing columns the
  schema expects, you will see warnings — these are informative and do
  not stop you from working with the data that is present.
- **Supplementary files are a separate path.** Re-opening (this
  vignette) needs only the three core files. Re-running the raw
  preprocessing pipeline
  ([`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md),
  [`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md),
  …) is a different workflow that operates on the raw ROXAS files, which
  are only available if they were submitted as supplementary resources
  (see
  [`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)).
