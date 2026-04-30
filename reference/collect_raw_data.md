# Collect raw cells and rings output data

Read and combine data from all raw cells and ring data files. The input
data frame with filenames plus data structure can be a `QWAimages`
object derived with
[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md),
or the output from the earlier
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md)
step.

## Usage

``` r
collect_raw_data(df_meta, roxas_version = NULL)
```

## Arguments

- df_meta:

  Data frame or `QWAimages` object containing filenames and data
  structure.

- roxas_version:

  The software used to create the files (`"roxas"` or `"roxas_ai"`).
  Required only if `df_meta` is not a `QWAimages` object.

## Value

A `QWAdata` object, ready to pass to
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md).

## Details

The resulting
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
should then be passed through
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
to add derived measures and ring quality flags.

## See also

[`build_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/build_QWAimages.md),
[`extract_data_structure()`](https://tria-db.github.io/rxs2tria/reference/get_structure_from_filenames.md),
[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
