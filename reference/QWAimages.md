# Create a QWAimages object

`QWAimages` is a subclass of `data.frame` that holds image-level
metadata for a QWA dataset. It is the required component of
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
and can be used as input for
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md).

## Usage

``` r
QWAimages(data, roxas_version = NULL)
```

## Arguments

- data:

  A data frame with image-level metadata. Is aligned with the required
  structure for QWAimages objects and validated against the
  corresponding JSON schema.

- roxas_version:

  The ROXAS software version: `"roxas"` or `"roxas_ai"`. If `NULL`,
  inferred from the `$software` column of `data`.

## Value

A `QWAimages` object with a `roxas_version` attribute.

## Details

Each row represents one image with its associated ROXAS (AI) output file
paths, hierarchical labels (site, tree, woodpiece, slide, image), EXIF
metadata, and ROXAS (AI) settings. It is typically constructed by
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md).

The `roxas_version` attribute records whether the data were produced by
classic ROXAS (`"roxas"`) or ROXAS AI (`"roxas_ai"`).

## See also

[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md),
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md),
[`read_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/read_QWAimages.md)
