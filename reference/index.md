# Package index

## Read raw ROXAS metadata

Functions for reading the metadata that can be extracted from the raw
ROXAS output files.

- [`get_roxas_files()`](https://tria-db.github.io/rxs2tria/reference/get_roxas_files.md)
  : Identify the ROXAS (AI) files in a directory
- [`get_structure_from_filenames()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
  [`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
  : Extract the data structure from file names
- [`collect_image_info()`](https://tria-db.github.io/rxs2tria/reference/collect_image_info.md)
  : Read and combine image exif data
- [`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
  : Read and combine ROXAS (AI) settings data

## Reading raw ROXAS output data

Functions for reading and writing ROXAS cells and rings output files.

- [`collect_raw_outputs()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_outputs.md)
  : Collect raw output data from multiple files
- [`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md)
  : Collect raw cells and rings output data

## QWAimages

S3 class QWAimages

- [`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md)
  : Build a QWAimages object from raw ROXAS (AI) metadata
- [`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md)
  : Validate a QWAimages object
- [`complete_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAimages.md)
  : Complete a QWAimages object
- [`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  : Create a QWAimages object
- [`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md)
  : Read a QWAimages object from a CSV file
- [`print(`*`<QWAimages>`*`)`](https://tria-db.github.io/rxs2tria/reference/summary.QWAimages.md)
  [`summary(`*`<QWAimages>`*`)`](https://tria-db.github.io/rxs2tria/reference/summary.QWAimages.md)
  : Print summary of a QWAimages object
- [`update_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/update_QWAimages.md)
  : Update QWA metadata for selected images
- [`write_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/write_QWAimages.md)
  : Write a QWAimages object to a CSV file.

## QWAmetadata

S3 QWAmetadata

- [`add_resources()`](https://tria-db.github.io/rxs2tria/reference/add_resources.md)
  : Add a resources table to a QWAmetadata object
- [`collect_resources()`](https://tria-db.github.io/rxs2tria/reference/collect_resources.md)
  : Collect resource file information from a directory
- [`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md)
  : Validate a QWAmetadata object
- [`complete_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAmetadata.md)
  : Complete a QWAmetadata object with all optional fields
- [`print(`*`<QWAmetadata>`*`)`](https://tria-db.github.io/rxs2tria/reference/print.QWAmetadata.md)
  [`summary(`*`<QWAmetadata>`*`)`](https://tria-db.github.io/rxs2tria/reference/print.QWAmetadata.md)
  : Print a QWAmetadata object
- [`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  [`as_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  : Create a QWAmetadata object
- [`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
  : Read a QWAmetadata object from a JSON file
- [`write_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAmetadata.md)
  : Write a QWAmetadata object to a JSON file

## QWAdata

S3 class QWAdata

- [`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
  : Validate a QWAdata object
- [`complete_flags()`](https://tria-db.github.io/rxs2tria/reference/complete_flags.md)
  : Add the initial flags columns to QWAdata
- [`complete_measures()`](https://tria-db.github.io/rxs2tria/reference/complete_measures.md)
  : Complete QWA measures
- [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
  : Complete a QWAdata object
- [`print(`*`<QWAdata>`*`)`](https://tria-db.github.io/rxs2tria/reference/print.QWAdata.md)
  [`summary(`*`<QWAdata>`*`)`](https://tria-db.github.io/rxs2tria/reference/print.QWAdata.md)
  : Print a QWAdata object
- [`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  [`as_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  : Create a QWAdata object
- [`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
  : Read a QWAdata object from CSV files
- [`update_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/update_QWAdata.md)
  : Update a QWAdata object with re-read raw data for selected images
- [`write_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAdata.md)
  : Write a QWAdata object to files

## QWAprofiles

Functions to calculate sector or band profiles from cell measurements.

- [`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_band_profiles.md)
  : Calculate band-wise radial profiles
- [`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)
  : Calculate sector-wise radial profiles
- [`check_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/check_QWAprofile.md)
  : Validate a QWAprofile object
- [`complete_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAprofile.md)
  : Add cell parameters to an existing QWAprofile object
- [`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  : Create a QWAprofile object
- [`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md)
  : Read a QWAprofile object from a CSV file
- [`print(`*`<QWAprofile>`*`)`](https://tria-db.github.io/rxs2tria/reference/summary.QWAprofile.md)
  [`summary(`*`<QWAprofile>`*`)`](https://tria-db.github.io/rxs2tria/reference/summary.QWAprofile.md)
  : Summarise a QWAprofile object
- [`write_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/write_QWAprofile.md)
  : Write a QWAprofile object to a CSV file

## rest

Other utility functions.

- [`launch_flags_app()`](https://tria-db.github.io/rxs2tria/reference/launch_flags_app.md)
  : Launch the ring flags Shiny app
- [`launch_metadata_app()`](https://tria-db.github.io/rxs2tria/reference/launch_metadata_app.md)
  : Launch the metadata Shiny app
- [`create_rwl()`](https://tria-db.github.io/rxs2tria/reference/create_rwl.md)
  : Create a Tucson (.rwl) file from ROXAS ring-width or profile data
