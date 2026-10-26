# rxs2tria (development version)

* The `check_*()` functions are the final checks before a submission: they warn about issues the TRIA admins would reject, gain `strict = FALSE` (with `TRUE`, they abort on the first issue) and return `TRUE`/`FALSE` invisibly.
  * `check_QWAimages()` also warns on a label in more than one parent, `outmost_year` in the future and missing `spatial_resolution`.
  * `check_QWAmetadata()` also warns on missing required components, duplicate `email`/`site_name`, not exactly one contact person, `author_nr` not numbering the authors from 1 to n, unknown country or species codes, species columns not matching their code, and `[ADD INFO` placeholders in `description`. Hierarchy tables not matching `$images` warn instead of aborting.
  * `check_QWAdata()` checks values against the schema and warns on duplicate cell keys, years with cells but without a rings row, and `cno` not matching the cells. Its `meta` argument is removed, see `check_consistency()`.
  * `check_supplementary()` warns instead of aborting on `"ok"` files missing from the archive, and warns on undeclared files. The manifest may carry extra columns.
* new `scale_for_tsap()` and `write_tsap()` export `extract_rwl()` series as tab-delimited TSAP import files (integer values, `NA` as `0`), with power-of-ten scaling for parameters on small scales.
* new `check_consistency()` checks `QWAimages`/`QWAmetadata`, `QWAdata` and `QWAprofile` objects against each other: images in both, rings labels matching the images, ring years after `outmost_year`, profile years with a rings row.
* All functions taking a QWA object abort on invalid objects: missing, mistyped or `NA` key columns, duplicate `image_label`s, duplicate (image, year) in the rings, duplicate profile keys. `collect_raw_data()` aborts on such keys in the raw output instead of warning.
* `QWAdata()` and `read_QWAdata()` no longer complete the year sequences of the rings or add `cno` (done by `complete_QWAdata()`), keep columns not in the schema, and warn instead of aborting on future years. `woodpiece_label` and `slide_label` are required in the rings.
* `QWAimages()` sets `$software` to the `roxas_version`, with a warning if values changed.
* `update_QWAmetadata()` keeps an existing `species_code` where it is `NA` in `images`.
* `calculate_band_profiles()` warns if cells have no rings row; `calculate_*_profiles(apply_exclusions = TRUE)` and `extract_rwl()` with `prf_data` abort in that case (run `complete_QWAdata()`). `QWAprofile()` and `read_QWAprofile()` warn on profiles without measurements, varying band widths and `sector_n` < 1.
* `compile_resources()` and `recompile_resources()` are renamed to `compile_supplementary()` and `recompile_supplementary()`; `vignette("resources")` is now `vignette("supplementary")`.
* JSON schemas are renamed: `json_schema/core/<component>.json`; the Shiny metadata app extensions to `app_<component>.json`.
* JSON schemas distinguish submission files (`submission_<component>.json`) from TRIA dataset files (`tria_<component>.json`), with shared definitions in `core/`. submission only: `$related`, `fname_*`, `ds_access`, `embargoed_until`; TRIA dataset files add `$datafiles`, `ds_label`, `url`, `doi`, `citation`, `dataset_version`.
* `QWAmetadata` objects carry a `submission` attribute, queried with new `is_submission()` and inferred from the components: TRIA dataset files have `$datafiles` (replaces `$resources`, which is no longer read) or a `ds_label`. Components are aligned, checked and written with the schemas of their stage, and `$related` is dropped from TRIA dataset files. `check_QWAimages()` gains `submission`. `complete_QWAmetadata()`, `update_QWAmetadata()` and `combine_QWAmetadata()` apply to submission files only. `write_QWAmetadata()` no longer writes absent components.
* Schema `colType`s are now `key`, `measure`, `derived` (recomputable arithmetic), `estimate` (`ew_lw`, `eww`, `lww`), `flag` (automatic ring flags), `meta_input` (filled in the metadata app, incl. `only_ew`, `img_comment`) and `qc_input` (set in the flags app: the optional ring flags, `comment`, `affected_tissue`, `comment_handled`). `cno` is a `measure`. The metadata component schemas carry `colType`s too.
* `complete_QWAimages()` and `complete_QWAmetadata()` add all optional columns except `qc_input` ones (`only_ew` and `img_comment` are now added). Optional columns added this way and missing optional columns found by the `check_*()` functions are reported as information, not as warnings.
* Empty values are written as `null` in JSON and as empty fields in CSV, never as `""` or `"NA"`. `write_QWAmetadata()`, `write_QWAdata()` and `write_QWAimages()` gain `drop_empty = TRUE`: optional `meta_input` and `qc_input` columns that are empty in all rows (`NA`, or `NA`/`FALSE` for flags) are not written. The metadata app exports via `write_QWAmetadata()`.
* `write_QWAprofile()` and the flags app also write empty values as empty fields. The flags app accepts `.csv.gz` rings and profile files.
* Fixes: `extract_rwl()` and `extract_ring_measures()` include the estimate columns `eww` and `lww`; `QWAprofile()` defaults to `profile_type = "sector"`; `check_QWAmetadata()` skips absent hierarchy tables instead of aborting.
* `write_QWAdata()` gains `keep_derived = TRUE`; with `FALSE`, derived columns are not written (recompute them with `complete_QWAdata()`).
* `ds_license` is required for public submissions and in TRIA dataset files only, `embargoed_until` for restricted submissions only; both may be absent otherwise.
* `update_QWAdata()` now synchronises the data with `meta`: new images are added and images not in `meta` are removed. Duplicate-ring flags are only recalculated for affected woodpiece-years, and with `reset_manual_flags = FALSE` manual `exclude_issues` edits are kept.
* `update_QWAdata()` signature is now `update_QWAdata(x, meta, imgs_to_reread = NULL, ...)`; `imgs_to_update` is renamed to `imgs_to_reread` (also in `update_QWAimages()`) and is optional in `update_QWAdata()`.
* new `update_QWAmetadata()` replaces the images of a `QWAmetadata` object and updates the sites, trees, woodpieces and slides tables (rows and counts) accordingly, keeping existing metadata.
* new `combine_QWAimages()`, `combine_QWAdata()` and `combine_QWAmetadata()` combine two datasets with distinct image labels (e.g. from separate input folders) into one.
* `exclude_issues` is now initialised from `incomplete_ring` only; missing rings are no longer excluded by default, as their zero width is a valid measurement. The `exclude_mode` argument of `complete_flags()`, `complete_QWAdata()`, `update_QWAdata()` and `check_QWAdata()` is deprecated; to also exclude missing rings, set `exclude_issues` after `complete_QWAdata()`.
* `complete_QWAprofile()` is removed; recalculate the profile with all parameters instead.
* new `extract_ring_measures()` returns the identifier and measurement columns of the rings data without excluded rings (`exclude_dupl`, `exclude_issues`), as input for further analyses.
* `calculate_sector_profiles()` and `calculate_band_profiles()` gain `apply_exclusions` and `tissue` arguments: with `apply_exclusions = TRUE`, the cells of excluded rings are removed before aggregating; `tissue = "affected"` removes only the earlywood or latewood cells of rings whose `affected_tissue` is `"ew"` or `"lw"`.
* `NA` values in `exclude_dupl` and `exclude_issues` now count as `FALSE` (ring not excluded), also in `extract_rwl()`.

# rxs2tria 0.1.3

* overhauled and finalised the extract rwl logic: new `extract_rwl()` with `scale_for_tucson()` and `rename_for_tucson()` (then write with `dplR::write.tucson()`), also added an `Export rwl` button to the flags Shiny app.
* overhauled the supplementary resources handling. Supplementary resources info is no longer part of the `QWAmetadata` object. `compile_resources()`, `recompile_resources()` and `check_supplementary()` replace the existing functions. The output suppl resource manifest is a slim table listing and describing each file, to be submitted alongside the zip.
* completed the ROXAS AI metadata attributes. renamed several ROXAS/ROXAS AI image metadata fields for clarity/consistency (`dbl_cwt_threshold` -> `cluster_dbl_cwt_threshold`, `maxrel_opp_cwt` -> `opposite_cwt_ratio_limit`, `relwidth_cwt_window` -> `relwidth_cwt_integration`, `comment` -> `img_comment`, `rxs_created_at` -> `meas_created_at`); backcomp: `read_QWAimages()`, `read_QWAmetadata()` and the flags app convert old names automatically. 
* new required dataset field `ds_title`; `read_QWAmetadata()` falls back to `ds_name` for older files.
* `read_QWAdata()` no longer has a `components` argument; pass only `file_cells` or `file_rings` to read a single component. With `dir`, a missing component now warns instead of aborting.
* `collect_settings_data()` now requires the image file paths for ROXAS AI as well (to get the actual image file size, not always correct in ROXAS AI metadata due to compression).
* `img_created_at` in `collect_settings_data()` is auto converted to datetime since it should be uniform in format. Now only `meas_created_at` for classical ROXAS data needs manual conversion.
* add vignette stubs for the Shiny apps and a new `vignette("submission")`.

# rxs2tria 0.1.2

* `build_QWAimages()` now comes with a safeguard against uncoverted datetime 
columns in `df_settings`.
* `collect_settings_data()` now accepts the data structure data frame directly (`df` argument), auto-detects `roxas_version` from the settings file names when not supplied, and no longer requires the file path vectors to be passed individually.
* `collect_resources()` now records an MD5 `checksum` and `size_bytes` for each file (used to verify integrity of supplementary files on upload). Expanded documentation of the resources step, including a new `vignette("resources")`.
* New `vignette("reopen-dataset")` documents how to re-open a downloaded TRIA dataset using the individual `read_*` functions.
* The flags app now accepts a `QWAmetadata` `.json` file directly as the images metadata input (its `$images` component is extracted), in addition to a `QWAimages` `.csv`.

# rxs2tria 0.1.1

* improved output of get_roxas_files to warn instead of abort for missing files -> now returns a df of the fnames
* improved shiny meta app to avoid costly ht rendering on image table, tweaks based on FB from GvA
* QWAdata validity checks based on json schema instead of hardcoded

# rxs2tria 0.1.0

* Initial (somewhat) stable release.
