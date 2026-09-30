# Read a QWAdata object from CSV files

Reads cells and/or rings from (compressed) CSV files. To load only one
component (e.g. to avoid reading a large cells file when only rings are
needed), provide only the corresponding `file_cells`/`file_rings`
argument.

## Usage

``` r
read_QWAdata(
  dir = NULL,
  file_cells = NULL,
  file_rings = NULL,
  dataset_name = NULL
)
```

## Arguments

- dir:

  Directory to search for cells and rings files. Both are read if found;
  if one is missing, a warning is issued and that component is omitted.
  Mutually exclusive with `file_cells`/`file_rings`.

- file_cells, file_rings:

  Explicit paths to the cells and rings CSV files. Either or both may be
  given; the omitted component is `NULL` in the returned
  [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object. Mutually exclusive with `dir`.

- dataset_name:

  Optional string to disambiguate when multiple matching files are found
  in `dir`.

## Value

A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object.

## See also

[`write_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAdata.md),
[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md)
