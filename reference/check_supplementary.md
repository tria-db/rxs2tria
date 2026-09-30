# Verify a supplementary resources manifest against a directory or archive

A pass/fail check confirming that your finalised manifest accurately
describes the content of the supplementary archive/directory and is
ready for submission. `suppl_res` must be a finalised manifest, i.e.
including the `status` column produced by
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)
or
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md).
Aborts if `suppl_res` lists a file with `status == "ok"` that isn't
found in `path`—a genuine inconsistency between the manifest and the
archive. Returns `FALSE` (with a warning pointing you back to
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md))
if any resource still needs review, or `TRUE` if everything is ready.

## Usage

``` r
check_supplementary(suppl_res, path, rxs_images)
```

## Arguments

- suppl_res:

  An existing resources data frame.

- path:

  Path to a directory to scan for files, or to an existing (non-nested)
  zip archive to read the file list from directly. Reading a zip archive
  requires the zip package, and does not extract any
  files—`fname_resource` is `NA` for resources found this way.

- rxs_images:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  object. Its `roxas_version` attribute is used to disambiguate
  resource-type patterns shared between classic ROXAS and ROXAS AI, and
  its `org_img_name`, `image_label`, `slide_label`, and
  `woodpiece_label` columns are used to auto-populate `linked_label`
  where appropriate.

## Value

`TRUE`/`FALSE` (invisibly), or aborts – see Description.

## See also

[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md),
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md),
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# verify a finalized manifest against an already-zipped submission
suppl_res <- vroom::vroom("output_data/supplementary_files.csv",
                          show_col_types = FALSE)
if (!check_supplementary(suppl_res, "supplementary_data.zip", rxs_images = my_images)) {
  suppl_res <- recompile_resources(suppl_res, "supplementary_data", rxs_images = my_images)
  # fix the flagged resource(s) in suppl_res, then check again
}
} # }
```
