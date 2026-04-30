# Create a QWAdata object

The S3 class `QWAdata` holds the quantitative wood anatomy (QWA)
measurements data produced from ROXAS output files, organised into two
components:

## Usage

``` r
QWAdata(cells = NULL, rings = NULL)

as_QWAdata(x)
```

## Arguments

- cells:

  Data frame with cell-level measurements (optional).

- rings:

  Data frame with ring-level measurements and flags (optional).

- x:

  A named list with any subset of the QWAdata components.

## Value

An object of class `QWAdata`.

## Details

- **`$cells`**: cell-level measurements (one row per cell).

- **`$rings`**: ring-level measurements, and possibly also quality flags
  (one row per annual ring per image).

At least one component must be provided. The typical workflow builds a
`QWAdata` object incrementally using
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md),
and
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md).
Metadata is kept separately as a
[QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
or
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object. Radial profiles are computed separately as
[QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
objects using
[`calculate_sector_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_sector_profiles.md)
or
[`calculate_band_profiles()`](https://tria-db.github.io/rxs2tria/reference/calculate_band_profiles.md).

## See also

[`QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md),
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md),
[`QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md),
[`collect_raw_data()`](https://tria-db.github.io/rxs2tria/reference/collect_raw_data.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md),
[`print.QWAdata()`](https://tria-db.github.io/rxs2tria/reference/print.QWAdata.md)
