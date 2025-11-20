
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxs2tria

<!-- badges: start -->

<!-- badges: end -->

The goal of `rxs2tria` is to prepare, quality check and harmonize a
collection of QWA images, corresponding ROXAS output files and
accompanying metadata for submission as a dataset to the QWA database.

**Note:** This package is currently under development and not yet stable
nor officially released. Version 1.0.0 will be made available under the
organizational github.

## Installation

You can install the development version of `rxs2tria` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tria-db/rxs2tria")

# or:
remotes::install_github('tria-db/rxs2tria')
```

## Usage

Use the package to extract all available metadata from the ROXAS output
files under a given path.

``` r
library(rxs2tria)

# set input and output paths and dataset name
path_in <- './path/to/ROXAS_output_data'
path_out <- './out'
dataset_name <- 'QWA_ex_dataset'
```

``` r
# get overview of data to be read and extract data structure from filenames
files <- get_roxas_files(path_in)
str(files)
```

``` r
# define the naming convention used for your files as a regex pattern
# e.g. the following means that your filenames are structured as follows:
# `{site}_{species}_{tree}_{slide}_{image}`
# since there is exactly one sample (woodpiece) per tree, no woodpiece identifier is included in the pattern
pattern <- "^(?<site>[:alnum:]+)_(?<species>[:alnum:]+)_(?<tree>[:alnum:].+)_(?<slide>[:alnum:]+)_(?<image>[:alnum:]+)$"

# extract the data structure from the file names using the specified pattern
df_structure <- extract_data_structure(files, pattern)
```

``` r
# read available metadata
df_meta <- collect_metadata_from_files(df_structure,
                                       roxas_version='classic')

# write extracted metadata to file
write.csv(df_meta, file.path(path_out, paste0(dataset_name, '_meta_extracted.csv')),
          row.names = FALSE)
```

To provide the additional metadata required (e.g. site and tree
information, author details, …) for the submission of a dataset, you can
use the interactive Shiny app which is part of this package.

The app is hosted on a WSL server and accessible via
[webapps.wsl.ch/QWAmeta](https://webapps.wsl.ch/QWAmeta/).

Or, if you want to run it locally, you can use the following command:

``` r
launch_metadata_app()
```

To load and preprocess the cell and ring measurements:

``` r
# read raw cells/rings data
QWA_data <- collect_raw_data(df_structure)

# clean raw data
QWA_data <- validate_QWA_data(QWA_data, df_meta)

# remove outliers
# NOTE: ROXAS does some threshold based outlier checks and assigns these a
# a negative value. Here, we replace these outliers with NAs
QWA_data <- remove_outliers(QWA_data)

# complete cell measures
QWA_data <- complete_cell_measures(QWA_data)

# save the cleaned data to files
write.csv(QWA_data$cells,
          file.path(path_out, paste0(dataset_name, '_cells.csv')),
          row.names = FALSE)

write.csv(QWA_data$rings,
          file.path(path_out, paste0(dataset_name, '_rings.csv')),
          row.names = FALSE)
```

To check the coverage and add ring quality flags and setting years to
exclude:

``` r
launch_coverage_app()

# TODO: add export functionality to coverage app
```
