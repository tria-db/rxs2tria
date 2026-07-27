
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxs2tria

<!-- badges: start -->

<!-- badges: end -->

rxs2tria is an R package to handle Quantitative Wood Anatomy (QWA) data,
specifically data generated with the [ROXAS](https://roxas.wsl.ch/en/)
or [ROXAS AI](https://github.com/roxas-ai/napari-roxas-ai/tree/released)
software. The package includes functionality to prepare, quality check
and harmonize a QWA dataset, consisting of QWA images, corresponding
cell and ring measurement files and accompanying metadata. The processed
dataset may then be submitted to the
[TRIA](https://webapps.wsl.ch/tria/) database for intra-annual and
anatomical tree-ring data.

**Note:** This package is currently under development and not yet stable
nor officially released.

# Installation

The package is (not yet) on CRAN. You can install `rxs2tria` from
[GitHub](https://github.com/tria-db/rxs2tria/):

``` r
pak::pak("tria-db/rxs2tria")

# or with another remote package installer, such as:
# devtools::install_github("tria-db/rxs2tria")
# remotes::install_github("tria-db/rxs2tria")
```

## Development version

To get the newest features and bug fixes, you can install the
development version:

``` r
pak::pak("tria-db/rxs2tria@develop")
```

## Troubleshooting

Ensure that the installer library (pak/devtools/remotes) is properly
installed. On Windows OS, you may need to install Rtools to build
packages from source. Run `pkgbuild::check_build_tool(debug=TRUE)` to
check for build tools, and restart RStudio/Positron after the
installation is completed.

# Quick start

The easiest way to get started is to copy the rxs2tria workflow template
to your working directory and fill in your dataset-specific paths and
settings, then run through each step.

``` r
library(rxs2tria)
file.copy(system.file("templates/prepare_rxs_dataset.R", package = "rxs2tria"), ".")
```

Follow the instructions in the template, or see the [workflow
vignette](https://tria-db.github.io/rxs2tria/articles/workflow.html) for
more details.

# Contributions

Contributions are welcome! Please feel free to open issues or submit
pull requests on the [GitHub
repository](https://github.com/tria-db/rxs2tria/).
