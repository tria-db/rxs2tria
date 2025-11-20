# Flag incomplete rings at tangential image borders.

This function determines which of the inner- and outermost rings of each
image are incomplete (i.e. extend across the image boundaries). For the
innermost rings, we rely on the position and shape of the estimated
inner ring border (see helper function `check_incomplete_innermost`).
For the outermost rings, we check the MRW value, since ROXAS only
estimates an MRW if it can detect an outer ring boundary.

## Usage

``` r
flag_incomplete_rings(df_rings_log, df_cells_all, df_meta)
```

## Arguments

- df_rings_log:

  the dataframe containing the rings data

- df_cells_all:

  the dataframe containing all cells data

- df_meta:

  the dataframe containing the metadata of the images (needed for the
  spatial resolution)

## Value

the input dataframe with additional logical columns 'incomplete_ring'
