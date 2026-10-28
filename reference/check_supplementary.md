# Verify a supplementary resources manifest against a directory or archive

A pass/fail check confirming that your finalised manifest accurately
describes the content of the supplementary archive/directory and is
ready for submission. A submission-time check: it applies to the
submitted supplementary CSV (with `status`), not to the supplementary
CSV of TRIA dataset files. `suppl_res` must be a finalised manifest,
i.e. including the `status` column produced by
[`compile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/compile_supplementary.md)
or
[`recompile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/recompile_supplementary.md).

## Usage

``` r
check_supplementary(suppl_res, path, rxs_images, strict = FALSE)
```

## Arguments

- suppl_res:

  An existing resources data frame.

- path:

  Path to a directory to scan for files, or to an existing (non-nested)
  zip archive to read the file list from directly. Reading a zip archive
  requires the zip package, and does not extract any
  files—`fname_resource` is `NA` for resources found this way. System
  files (e.g. `.DS_Store`, `Thumbs.db`, `__MACOSX/` entries) are
  skipped.

- rxs_images:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object. Its `roxas_version` attribute is used to disambiguate
  resource-type patterns shared between classic ROXAS and ROXAS AI, and
  its `org_img_name`, `image_label`, `slide_label`, and
  `woodpiece_label` columns are used to auto-populate `linked_label`
  where appropriate.

- strict:

  If `TRUE`, abort on the first issue instead of warning.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## Details

Warns (or, with `strict = TRUE`, aborts) on:

- files with `status == "ok"` in `suppl_res` not found in `path`;

- files in `path` not declared in `suppl_res` (junk files aside);

- resources that still need review (fix them and run
  [`recompile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/recompile_supplementary.md));

- schema issues of the manifest.

Junk files and manual corrections in `suppl_res` are reported as
information.

## See also

[`recompile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/recompile_supplementary.md),
[`compile_supplementary()`](https://tria-db.github.io/rxs2tria/reference/compile_supplementary.md),
[`vignette("supplementary")`](https://tria-db.github.io/rxs2tria/articles/supplementary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# verify a finalised manifest against an already-zipped submission
suppl_res <- vroom::vroom("output_data/supplementary_files.csv",
                          show_col_types = FALSE)
if (!check_supplementary(suppl_res, "supplementary_data.zip", rxs_images = my_images)) {
  suppl_res <- recompile_supplementary(suppl_res, "supplementary_data", rxs_images = my_images)
  # fix the flagged resource(s) in suppl_res, then check again
}
} # }
```
