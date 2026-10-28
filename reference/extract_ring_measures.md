# Extract ring measurements without excluded rings

Returns the ring-level measurements of a `QWAdata$rings` data frame
without the excluded rings, i.e. duplicate rings not selected as the
preferred copy (`exclude_dupl`) and rings flagged for exclusion
(`exclude_issues`). The result has one row per woodpiece and year; years
of removed rings are absent, so the series may contain gaps.

## Usage

``` r
extract_ring_measures(df_rings)
```

## Arguments

- df_rings:

  A data frame containing ROXAS ring-level measurements and logical flag
  columns (the `$rings` component of a `QWAdata` object, after calling
  [`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)
  to instantiate the flag columns).

## Value

A data frame with the identifier columns (`woodpiece_label`,
`slide_label`, `image_label`, `year`) and the measurement columns of
`df_rings` (e.g. `cno`, `mrw`, `eww`, `lww`).

## Details

Only the identifier and measurement columns are kept; flag and comment
columns are dropped. The result is meant as input for further analyses.
To change which rings are excluded, edit the flags in the full `QWAdata`
object and extract again.

## See also

[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md),
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_meas <- extract_ring_measures(QWA_data$rings)
} # }
```
