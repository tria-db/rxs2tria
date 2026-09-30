# Scale an rwl object for Tucson-format writing

[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
stores values as integers at a fixed precision, giving 5 digits of
usable range. Since QWA parameters can be on very different scales and
units (e.g. μm²) to the mm ring widths dplR expects, this function
scales an `rwl` object (as returned by
[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md))
by a power-of-ten factor. By default, the factor is chosen automatically
to make full use of that range, for a given `prec`.

## Usage

``` r
scale_for_tucson(rwl, prec = 0.001, scaling = NULL)
```

## Arguments

- rwl:

  A dplR `rwl` object, e.g. as returned by
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md).

- prec:

  Numeric, the precision
  [`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
  will be called with: either `0.001` (default) or `0.01`.

- scaling:

  Numeric power of ten to scale by, or `NULL` (default) to determine the
  factor automatically.

## Value

The scaled `rwl` object, with the applied factor as attribute
`"scaling"`.

## Details

For ring-width parameters in μm (e.g. `mrw`, `eww`, `lww`), use
`scaling = 0.001` to convert the values to mm, as conventionally
expected for the input object of
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html).
Auto-scaling would instead pick the factor maximising the represented
digits at the selected `prec`, which does not generally correspond to
mm.

A manual `scaling` must be a power of ten, and must not push any value
beyond the Tucson range at the given `prec`; otherwise the function
aborts and suggests the largest factor that fits.

The applied factor is stored in the `"scaling"` attribute of the
returned `rwl` object, and recovers the original values: dividing the
scaled `rwl` object, or values re-read with
[`dplR::read.tucson()`](https://rdrr.io/pkg/dplR/man/read.tucson.html),
by `scaling` gives back the original values. The *raw* integers stored
in an `.rwl` file created with
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
are additionally scaled by `1 / prec`, so to recover the original values
directly from the raw digits, apply `* prec / scaling`. An `rwl` object
that already has a `"scaling"` attribute is not scaled again.

The attribute is kept by
[`rename_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/rename_for_tucson.md),
so both functions can be applied in either order, but is dropped by most
other operations (e.g. subsetting, arithmetic, dplR functions). Apply
them as the last steps before writing.

Note that
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
writes negative values, and at `prec = 0.001` also values rounding to
zero, as missing values.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ring widths in mm
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
scaled <- scale_for_tucson(rwl, scaling = 0.001)
dplR::write.tucson(scaled, fname = "mrw.rwl", prec = 0.001)

# Other parameters, auto-scaled
rwl <- extract_rwl(df_rings = QWA_data$rings, param = "la_q90",
                   prf_data = prf_sector, sector = 5)
scaled <- scale_for_tucson(rwl, prec = 0.001)
attr(scaled, "scaling")
dplR::write.tucson(scaled, fname = "la_q90.rwl", prec = 0.001)
} # }
```
