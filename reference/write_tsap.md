# Write an rwl object as a TSAP import file

Writes an `rwl` object (as returned by
[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md))
as a tab-delimited text file for import into TSAP: the first column
`Year`, followed by one column per series. Missing values are written as
`0` and values are rounded to integers.

Ring widths in μm (e.g. `mrw`) can be written as they are. Scale
parameters on smaller scales with
[`scale_for_tsap()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tsap.md)
first; the function warns if non-zero values round to `0`, and aborts on
values beyond the integer range.

## Usage

``` r
write_tsap(rwl, file, overwrite = TRUE)
```

## Arguments

- rwl:

  A dplR `rwl` object, e.g. as returned by
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
  or
  [`scale_for_tsap()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tsap.md).

- file:

  Path of the output file (e.g. `"mrw_tsap.txt"`).

- overwrite:

  Logical, whether to overwrite an existing `file`.

## Value

The written table invisibly: a tibble with an integer `Year` column and
one integer column per series.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ring widths in μm
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
write_tsap(rwl, "mrw_tsap.txt")

# Other parameters, auto-scaled
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "cwtrad_mean",
                   prf_data = prf_sector, sector = 5)
write_tsap(scale_for_tsap(rwl), "cwtrad_mean_tsap.txt")
} # }
```
