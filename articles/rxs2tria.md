# rxs2tria

## Introduction to rxs2tria

**Note:** This package is currently under development and not yet stable
nor officially released.

`rxs2tria` provides tools to preprocess and prepare Quantitative Wood
Anatomy (QWA) data — specifically data generated with
[ROXAS](https://roxas.wsl.ch/en/) or its successor [ROXAS
AI](https://github.com/roxas-ai/napari-roxas-ai/tree/released) — for
upload to the [TRIA database](https://webapps.wsl.ch/tria/). The package
covers the full workflow from locating raw output files, through quality
checking and computing derived measures, to assembling a complete
dataset submission.

### Installation

You can install the development version of `rxs2tria` from
[GitHub](https://github.com/tria-db/rxs2tria/) with:

``` r

pak::pak("tria-db/rxs2tria")
# pak::pak("tria-db/rxs2tria@develop") for the develop branch
```

### Quick start

The fastest way to get started is to copy the workflow template to your
working directory and fill in your dataset-specific paths and settings:

``` r

file.copy(system.file("templates/prepare_rxs_dataset.R",
                      package = "rxs2tria"), ".")
```

Follow the instructions in the template, or see the [worklflow
vignette](https://tria-db.github.io/rxs2tria/articles/workflow.md) for
more details.
