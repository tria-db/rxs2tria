# Scale an rwl object for TSAP export

[`write_tsap()`](https://tria-db.github.io/rxs2tria/reference/write_tsap.md)
writes values rounded to integers, as TSAP expects ring widths in μm
(typically 300–2000). QWA parameters on smaller scales (e.g. cell wall
thickness, or relative measures well below 1) would lose most or all of
their information. This function scales an `rwl` object (as returned by
[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md))
by a power-of-ten factor. By default, the factor is chosen automatically
as the largest that keeps all values at most 9999 (4 integer digits).

## Usage

``` r
scale_for_tsap(rwl, scaling = NULL)
```

## Arguments

- rwl:

  A dplR `rwl` object, e.g. as returned by
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md).

- scaling:

  Numeric power of ten to scale by, or `NULL` (default) to determine the
  factor automatically.

## Value

The scaled `rwl` object, with the applied factor as attribute
`"scaling"`.

## Details

Ring-width parameters in μm (e.g. `mrw`, `eww`, `lww`) need no scaling.
Auto-scaling would instead scale series with all values below 1000 by 10
or more.

A manual `scaling` must be a power of ten and keep all values within the
integer range (at most `.Machine$integer.max`). The applied factor is
stored in the `"scaling"` attribute of the returned `rwl` object;
dividing the exported values by `scaling` gives back the original values
(up to rounding). An `rwl` object that already has a `"scaling"`
attribute is not scaled again. The attribute is dropped by most other
operations (e.g. subsetting, arithmetic, dplR functions), so apply this
function as the last step before writing.

## See also

[`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md)
for Tucson-format writing.

## Examples

``` r
if (FALSE) { # \dontrun{
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "cwtrad_mean",
                   prf_data = prf_sector, sector = 5)
scaled <- scale_for_tsap(rwl)
attr(scaled, "scaling")
write_tsap(scaled, "cwtrad_mean_tsap.txt")
} # }
```
