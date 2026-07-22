# rxs2tria (development version)

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
