# Build a QWAimages object from raw ROXAS (AI) metadata

Combines the extracted ROXAS / ROXAS AI file names plus data structure
(from
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md))
and ROXAS settings and image exif data (from
[`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md))
into a
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object.

## Usage

``` r
build_QWAimages(df_structure, df_settings)
```

## Arguments

- df_structure:

  Data frame with all input file names and data structure.

- df_settings:

  Data frame with extracted ROXAS (AI) settings data. The
  `img_created_at` and `rxs_created_at` columns (if present and not
  entirely `NA`) must already be converted to `POSIXct`, e.g. with
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
  (see the `workflow` vignette).

## Value

A [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object with the joined image-level metadata.

## Details

This is the typical starting point of the `prepare_rxs_dataset`
workflow. The resulting
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
can be passed directly to
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
(to read the cell and ring measurements) and to the metadata Shiny app
([`launch_metadata_app()`](https://tria-db.github.io/rxs2tria/reference/launch_metadata_app.md),
where one can enrich it with site-, tree-, and dataset-level metadata to
produce a full
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
object).

## See also

[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md),
[`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md),
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md),
[`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md)
