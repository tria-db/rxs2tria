# Plot overview of annual coverage of images

For a single tree, each image covers a span of years. This function
allows to visualize which annual rings are covered by which image and
which rings have been flagged due to which issues.

## Usage

``` r
plot_woodpiece_coverage(woodpiece, df_rings)
```

## Arguments

- woodpiece:

  the woodpiece (core) for which the plot should be created

- df_rings:

  the dataframe containing the rings data (including flags)
