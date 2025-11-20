# Flag duplicate rings due to overlapping images from the same woodpiece

This function identifies duplicate rings, i.e. the same annual ring
being captured in two or more images from the same woodpiece due to
overlaps. It also ranks the duplicated rings by the number of cells they
contain (in decreasing order), since for chronologies, we generally want
to keep only the ring with the most cells (rank 1).

## Usage

``` r
flag_duplicate_rings(df_rings_log)
```

## Arguments

- df_rings_log:

  the dataframe containing the rings data

## Value

the input dataframe with additional logical column 'duplicate_ring' and
integer column 'duplicate_sel' with the selected ring among duplicates
