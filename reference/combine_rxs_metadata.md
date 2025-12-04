# Combine raw ROXAS metadata

Combine the extracted ROXAS filenames plus data structures (from
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)),
image exif data (from
[`collect_image_info()`](https://tria-db.github.io/rxs2tria/reference/collect_image_info.md)),
and ROXAS settings data (with converted dates, cf.
[`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
and
[`convert_settings_dates()`](https://tria-db.github.io/rxs2tria/reference/convert_settings_dates.md))
into one raw metadata dataframe.

## Usage

``` r
combine_rxs_metadata(df_structure, df_images, df_settings)
```

## Arguments

- df_structure:

  Dataframe with all input filenames and data structure.

- df_images:

  Dataframe with extracted image exif data.

- df_settings:

  Dataframe with extracted ROXAS settings data.

## Value

A single dataframe consisting of the joined input dataframes.
