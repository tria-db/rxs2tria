# Reconcile and re-check a supplementary resources manifest

Re-scans `path` and reconciles the result against `suppl_res`, an
existing manifest (e.g. the output of a previous
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md)/
`recompile_resources()` call, or one read back in from a CSV). Use this
after making manual edits and/or adding or removing files in `path` to
create a re-checked manifest up-to-date with the contents of `path`.

`suppl_res` is treated as the source of truth wherever it overlaps with
what's actually found in `path`, see
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for details.

## Usage

``` r
recompile_resources(suppl_res, path, rxs_images, add_new_files = FALSE)
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

- add_new_files:

  Logical; if `TRUE`, files found in `path` that aren't declared in
  `suppl_res` are added as new, freshly-typed rows instead of being left
  out. Default `FALSE`.

## Value

A data frame, see
[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md).
Aborts instead of returning if `suppl_res` lists a file with
`status == "ok"` that isn't found in `path`.

## See also

[`compile_resources()`](https://tria-db.github.io/rxs2tria/reference/compile_resources.md),
[`check_supplementary()`](https://tria-db.github.io/rxs2tria/reference/check_supplementary.md),
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# re-run after adding more files to path: manual edits on suppl_res survive;
# add_new_files = TRUE also picks up the new ones
suppl_res <- recompile_resources(suppl_res, "path/to/submission_files",
                                 rxs_images = my_images, add_new_files = TRUE)
} # }
```
