# Rename rwl series to short Tucson-compatible series IDs

The Tucson format limits series IDs to 6–8 characters out of `A-Z`,
`a-z` and `0-9` (see `long.names` in
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)),
which the `woodpiece_label`s used as column names by
[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
usually exceed. `rename_for_tucson()` replaces them by short series IDs
derived from the data structure, instead of the generic truncation
applied by
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html).

`make_short_series_ids()` derives the underlying mapping from
`woodpiece_label` to short series ID.

## Usage

``` r
rename_for_tucson(rwl, df_structure, long.names = FALSE)

make_short_series_ids(df_structure, max_chars = 8)
```

## Arguments

- rwl:

  A dplR `rwl` object with `woodpiece_label`s as column names, e.g. as
  returned by
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md).

- df_structure:

  A data frame with the data structure columns `woodpiece_label`,
  `site_label` and optionally `species_code`, e.g. a
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object.

- long.names:

  Logical, the value
  [`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
  will be called with: `FALSE` (default) allows 6 characters, `TRUE`
  allows 8 characters (7 if any year is before -999 or after 9999).

- max_chars:

  Integer, the maximum number of characters per series ID.

## Value

- `rename_for_tucson()`: the `rwl` object with short series IDs as
  column names, and a data frame with columns `woodpiece_label` and
  `series_id` (in column order) as attribute `"mapping"`.

- `make_short_series_ids()`: a tibble with columns `woodpiece_label` and
  `series_id`, one row per woodpiece in `df_structure`.

## Details

The base ID of a woodpiece is its `woodpiece_label` without the site and
species prefixes, reduced to the allowed characters (e.g.
`YAM_LASI_122_a` becomes `122a`). The first of the following variants
that yields unique IDs of at most `max_chars` characters is used for all
series:

1.  site label + base ID (e.g. `YAM122a`),

2.  base ID only (e.g. `122a`),

3.  species code + base ID (e.g. `LASI122a`).

If none of them does, the function aborts.

The mapping is stored in the `"mapping"` attribute of the returned `rwl`
object. The attribute is kept by
[`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md),
so both functions can be applied in either order, but is dropped by most
other operations (e.g. subsetting, arithmetic, dplR functions). Apply
them as the last steps before writing. An `rwl` object that already has
a `"mapping"` attribute is not renamed again.

## Examples

``` r
if (FALSE) { # \dontrun{
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
rwl_out <- rwl |>
  rename_for_tucson(QWA_images, long.names = TRUE) |>
  scale_for_tucson(scaling = 0.001)
attr(rwl_out, "mapping")
dplR::write.tucson(rwl_out, fname = "mrw.rwl", prec = 0.001,
                   long.names = TRUE)
} # }
```
