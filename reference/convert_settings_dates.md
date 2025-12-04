# Handle date conversion for ROXAS settings created at dates

The Date created field in the raw ROXAS settings files may have
different formats depending on the ROXAS version and user locale.

## Usage

``` r
convert_settings_dates(
  date_strings,
  orders = c("%d.%m.%Y %H:%M:%S", "%d/%m/%Y %H:%M"),
  tz = Sys.timezone(),
  exact = TRUE
)
```

## Arguments

- date_strings:

  Vector of date strings to be converted.

- orders:

  One or more date formats to be used for parsing, see
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html).

- tz:

  Timezone for the converted dates (default: system timezone).

- exact:

  Whether to enforce exact matching of the provided format(s), or allow
  lubridate some flexibility to guess.

## Value

A vector of POSIXct dates.
