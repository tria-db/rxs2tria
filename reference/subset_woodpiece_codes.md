# Include or exclude woodpiece codes from data collection

This function filters the data structure dataframe based on the
woodpiece codes provided

## Usage

``` r
subset_woodpiece_codes(
  df_structure,
  include_codes = NULL,
  exclude_codes = NULL
)
```

## Arguments

- df_structure:

  The dataframe containing all input filenames and data structure.

- include_codes:

  EITHER provide a vector of the woodpiece codes to be included

- exclude_codes:

  OR provide a vector of the woodpiece codes to be excluded

## Value

filtered df_structure.
