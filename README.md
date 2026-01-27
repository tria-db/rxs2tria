
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxs2tria

<!-- badges: start -->

<!-- badges: end -->

`rxs2tria` is an `R` package to handle Quantitative Wood Anatomy (QWA)
data, specifically data generated with the
[ROXAS](https://roxas.wsl.ch/en/) software or its successor [ROXAS
AI](https://github.com/roxas-ai/napari-roxas-ai/tree/released) *(under
development)*. The package includes functionality to prepare, quality
check and harmonize a QWA dataset, consisting of QWA images,
corresponding ROXAS output files and accompanying metadata. The
processed dataset may then be submitted to the
[TRIA](https://webapps.wsl.ch/tria/) database for intra-annual and
anatomical tree-ring data.

**Note:** This package is currently under development and not yet stable
nor officially released.

## Installation

You can install the development version of `rxs2tria` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tria-db/rxs2tria")

# or:
remotes::install_github('tria-db/rxs2tria')
```

## Getting started

See the
[vignette](https://tria-db.github.io/rxs2tria/articles/rxs2tria.html)
for a detailed introduction and example workflow. You can access it
locally with:

``` r
vignette("rxs2tria")
```

## Contributions

Contributions are welcome! Please feel free to open issues or submit
pull requests on the [GitHub
repository](https://github.com/tria-db/rxs2tria/).
