# Helper function to check if an innermost ring is incomplete

Given a dataframe with cells data from a specific year (usually the
innermost one), this function checks whether the estimated inner ring
border is very close to 0 (i.e. the image border) and also very straight
(i.e. very similar at both sides of the image). This indicates that the
ring is cut-off and ROXAS used the image border instead of the true ring
border to estimate MRW.

## Usage

``` r
check_incomplete_innermost(cells.innermost, res)
```

## Arguments

- cells.innermost:

  a dataframe containing the cells data for the innermost ring

- res:

  the spatial resolution of the image in microns per pixel

## Value

a logical indicating whether the true inner ring border lies outside the
image border and the ring is thus incomplete

TODO: compare my method with GvA's original one, check thresholds
