# Update QWA metadata for selected images

Re-reads the settings/metadata files for the given images and replaces
the corresponding rows in the existing
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object. Useful when source files have changed after initial import.

## Usage

``` r
update_QWAimages(meta, imgs_to_update, settings_date_orders)
```

## Arguments

- meta:

  a
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object

- imgs_to_update:

  character vector of `image_label` values to update; must be a subset
  of `meta$image_label`

- settings_date_orders:

  Character; date order string(s) passed to
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
  to parse the `rxs_created_at` field

## Value

An updated
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object
