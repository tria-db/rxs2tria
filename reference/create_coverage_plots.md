# Create coverage plots for all trees

Function to create coverage plots for all unique treecodes in the
provided rings df (usually QWA_data\$rings) in a for loop. Plots can be
shown and/or saved to a specified path.

## Usage

``` r
create_coverage_plots(df_rings, save_plot = TRUE, path_out = "./")
```

## Arguments

- df_rings:

  rings dataframe

- save_plot:

  should the plot be written to disk under path_out

- path_out:

  path where the plot should be saved.
