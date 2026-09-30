# Compile a supplementary resources manifest

Scans a directory or zip archive for supplementary resource files (e.g.
original and annotated images, raw ROXAS outputs, reference RWL series,
etc.) and returns a data frame listing each file together with its
inferred resource type and the hierarchical entity it belongs to (e.g.
the corresponding image_label or woodpiece_label; if applicable).

This function is only relevant for TRIA submissions which include
supporting materials in addition to the required
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md) and
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
files. It compiles a manifest of the supplementary resources—it does not
move, copy, rename, or compress any files. TRIA limits the kind of
supplementary files which can be submitted (see
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for a full list), for any other submitted files the contributor is
required to provide a description of what it is and why it is relevant.
Thus the purpose of the manifest is to ensure that only the *right*
supplementary files are included in the submitted zip, and that these
files can all be correctly identified.

The automatic inference of resource type and linked label leverages the
suffix file name conventions used by ROXAS and ROXAS AI plus the
original image file names. The results are checked for validity (e.g. if
there were files for which the resource type or label could not be
inferred), with warnings raised in case of issues. Two additional
columns (`status`/`note`) are generated during the validity check and
describe how the resource would be treated upon submission (`"ok"`,
`"review"`, `"ignore"`) and why.

Inspect the returned data frame and address any raised warnings. You may
want to manually change the contents of the input directory
(adding/removing/renaming files), or edit the data frame directly
(editing the values for `resource_type`, `linked_level`, `linked_label`
or `description`). **Do not** edit the derived `status`/`note` columns.
After your edits, use
[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md)
to reconcile the updated manifest against the current contents of `path`
and re-derive the `status`/`note` columns without losing any manual
edits.

To verify a finalized manifest against a directory or zip archive, ses
[`check_supplementary()`](https://tria-db.github.io/rxs2tria/reference/check_supplementary.md).
Then save the resulting table to a CSV (e.g.
[`vroom::vroom_write()`](https://vroom.tidyverse.org/reference/vroom_write.html))
and submit it alongside the supplementary zip archive.

See
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)
for a worked example and the complete list of recognised resource types.

## Usage

``` r
compile_resources(path, rxs_images)
```

## Arguments

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

A data frame with one row per file and columns:

- `resource_name`: name and path of the file relative to `path`

- `description`: normally `NA`, can be filled manually if required to
  provide justification for "other" supplementary files that should be
  included in the upload

- `resource_type`: inferred resource type string (see Details).

- `linked_level`: hierarchy level for this type (e.g. `"dataset"`,
  `"woodpiece"`, or `"analysis"`).

- `linked_label`: label of the linked entity, auto-filled from where
  possible, otherwise `NA` (fill in manually if required).

- `fname_resource`: absolute path to the file, or `NA` for a resource
  read from a zip archive.

- `status`, `note`: status is `"ok"`, `"review"`, or `"ignore"`, note
  gives further details.

Returns `NULL` if no files are found in `path`.

## See also

[`recompile_resources()`](https://tria-db.github.io/rxs2tria/reference/recompile_resources.md),
[`check_supplementary()`](https://tria-db.github.io/rxs2tria/reference/check_supplementary.md),
[`vignette("resources")`](https://tria-db.github.io/rxs2tria/articles/resources.md)

## Examples

``` r
if (FALSE) { # \dontrun{
suppl_res <- compile_resources("path/to/submission_files", rxs_images = my_images)
# review and make changes are required, then persist as a standalone CSV
# to submit alongside the resources zip:
vroom::vroom_write(suppl_res, "output_data/submission_files_resources.csv")
} # }
```
