# Changelog

## rxs2tria 0.1.3

- overhauled and finalised the extract rwl logic: new
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
  with
  [`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md)
  and
  [`rename_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/rename_for_tucson.md)
  (then write with
  [`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)),
  also added an `Export rwl` button to the flags Shiny app.
- overhauled the supplementary resources handling. Supplementary
  resources info is no longer part of the `QWAmetadata` object.
  [`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md),
  [`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
  and
  [`check_supplementary()`](https://tria-db.github.io/rxs2tria/reference/check_supplementary.md)
  replace the existing functions. The output suppl resource manifest is
  a slim table listing and describing each file, to be submitted
  alongside the zip.
- completed the ROXAS AI metadata attributes. renamed several
  ROXAS/ROXAS AI image metadata fields for clarity/consistency
  (`dbl_cwt_threshold` -\> `cluster_dbl_cwt_threshold`, `maxrel_opp_cwt`
  -\> `opposite_cwt_ratio_limit`, `relwidth_cwt_window` -\>
  `relwidth_cwt_integration`, `comment` -\> `img_comment`,
  `rxs_created_at` -\> `meas_created_at`); backcomp:
  [`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md),
  [`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
  and the flags app convert old names automatically.
- new required dataset field `ds_title`;
  [`read_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAmetadata.md)
  falls back to `ds_name` for older files.
- [`read_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/read_QWAdata.md)
  no longer has a `components` argument; pass only `file_cells` or
  `file_rings` to read a single component. With `dir`, a missing
  component now warns instead of aborting.
- [`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
  now requires the image file paths for ROXAS AI as well (to get the
  actual image file size, not always correct in ROXAS AI metadata due to
  compression).
- `img_created_at` in
  [`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
  is auto converted to datetime since it should be uniform in format.
  Now only `meas_created_at` for classical ROXAS data needs manual
  conversion.
- add vignette stubs for the Shiny apps and a new
  [`vignette("submission")`](https://tria-db.github.io/rxs2tria/articles/submission.md).

## rxs2tria 0.1.2

- [`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md)
  now comes with a safeguard against uncoverted datetime columns in
  `df_settings`.
- [`collect_settings_data()`](https://tria-db.github.io/rxs2tria/reference/collect_settings_data.md)
  now accepts the data structure data frame directly (`df` argument),
  auto-detects `roxas_version` from the settings file names when not
  supplied, and no longer requires the file path vectors to be passed
  individually.
- `collect_resources()` now records an MD5 `checksum` and `size_bytes`
  for each file (used to verify integrity of supplementary files on
  upload). Expanded documentation of the resources step, including a new
  [`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md).
- New
  [`vignette("reopen-dataset")`](https://tria-db.github.io/rxs2tria/articles/reopen-dataset.md)
  documents how to re-open a downloaded TRIA dataset using the
  individual `read_*` functions.
- The flags app now accepts a `QWAmetadata` `.json` file directly as the
  images metadata input (its `$images` component is extracted), in
  addition to a `QWAimages` `.csv`.

## rxs2tria 0.1.1

- improved output of get_roxas_files to warn instead of abort for
  missing files -\> now returns a df of the fnames
- improved shiny meta app to avoid costly ht rendering on image table,
  tweaks based on FB from GvA
- QWAdata validity checks based on json schema instead of hardcoded

## rxs2tria 0.1.0

- Initial (somewhat) stable release.
