# Read a QWAdata object from CSV files

Reads cells and rings from (compressed) CSV files. Use the `components`
argument to load only a subset, e.g. to avoid reading a large cells file
when only rings are needed.

## Usage

``` r
read_QWAdata(
  dir = NULL,
  file_cells = NULL,
  file_rings = NULL,
  dataset_name = NULL,
  components = c("cells", "rings")
)
```

## Arguments

- dir:

  Directory to search for cells and rings files. Mutually exclusive with
  `file_cells`/`file_rings`.

- file_cells, file_rings:

  Explicit paths to the cells and rings CSV files. Both must be provided
  together. Mutually exclusive with `dir`.

- dataset_name:

  Optional string to disambiguate when multiple matching files are found
  in `dir`.

- components:

  Character vector of components to read. Any subset of
  `c("cells", "rings")`. Defaults to `c("cells", "rings")`. Omitted
  components are `NULL` in the returned
  [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

## Value

A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object.

## See also

[`write_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/write_QWAdata.md),
[`read_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/read_QWAprofile.md)
