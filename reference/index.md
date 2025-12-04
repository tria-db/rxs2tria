# Package index

## Read raw ROXAS metadata

Functions for reading the metadata that can be extracted from the raw
ROXAS output files.

- [`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md)
  : Identify ROXAS data files
- [`get_structure_from_filenames()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
  [`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
  : Extract the data structure from file names
- [`collect_image_info()`](https://tria-db.github.io/rxs2tria/reference/collect_image_info.md)
  : Read and combine image exif data
- [`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
  : Read and combine ROXAS settings data
- [`convert_settings_dates()`](https://tria-db.github.io/rxs2tria/reference/convert_settings_dates.md)
  : Handle date conversion for ROXAS settings created at dates
- [`combine_rxs_metadata()`](https://tria-db.github.io/rxs2tria/reference/combine_rxs_metadata.md)
  : Combine raw ROXAS metadata

## Reading raw ROXAS output data

Functions for reading and writing ROXAS cells and rings output files.

- [`collect_cells_data()`](https://tria-db.github.io/rxs2tria/reference/collect_cells_data.md)
  : Collect raw cells output data
- [`collect_rings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_rings_data.md)
  : Collect raw rings output data
- [`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
  : Collect raw cells and rings output data

## rest

Other utility functions.

- [`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
  : Read already processed QWA data from CSV files
- [`launch_coverage_app()`](https://tria-db.github.io/rxs2tria/reference/launch_coverage_app.md)
  : Launch the coverage Shiny app
- [`launch_metadata_app()`](https://tria-db.github.io/rxs2tria/reference/launch_metadata_app.md)
  : Launch the metadata Shiny app
- [`resolve_schema()`](https://tria-db.github.io/rxs2tria/reference/resolve_schema.md)
  : Read and resolve JSON Schema from file
- [`plot_woodpiece_coverage()`](https://tria-db.github.io/rxs2tria/reference/plot_woodpiece_coverage.md)
  : Plot overview of annual coverage of images
- [`create_coverage_plots()`](https://tria-db.github.io/rxs2tria/reference/create_coverage_plots.md)
  : Create coverage plots for all trees
- [`remove_outliers()`](https://tria-db.github.io/rxs2tria/reference/remove_outliers.md)
  : Remove outliers in cells and rings data
- [`validate_QWA_data()`](https://tria-db.github.io/rxs2tria/reference/validate_QWA_data.md)
  : Validate the raw QWA data
