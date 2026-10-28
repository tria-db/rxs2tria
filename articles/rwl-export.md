# Interoperability with other formats

``` r

library(rxs2tria)
```

## Overview

Annually resolved QWA parameters are time series like ring widths, and
can be analysed with standard dendrochronological tools. `rxs2tria`
currently connects to them at three levels:

- **dplR**:
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
  builds an `rwl` object (years as row names, one column per woodpiece)
  from a chosen parameter from the rings data or a sector profile at a
  givensector. This is the format established by the dplR package, and
  can thus be used as the basis for all its capabilities (e.g. for
  detrending and chronology building).
- **Tucson `.rwl` files**: the classic exchange format, read e.g. by
  COFECHA, is restrictive about series IDs and value ranges.
  [`rename_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/rename_for_tucson.md)
  and
  [`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md)
  prepare the `rwl` object for
  [`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html).
- **TSAP**:
  [`write_tsap()`](https://tria-db.github.io/rxs2tria/reference/write_tsap.md)
  writes a tab-delimited file for import into TSAP, after
  [`scale_for_tsap()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tsap.md)
  where needed.

------------------------------------------------------------------------

## Creating a dplR rwl object

### Prerequisites

[`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
takes the `$rings` component of a `QWAdata` object after
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md),
which adds the ring flags the export relies on (see
[`vignette("workflow")`](https://tria-db.github.io/rxs2tria/articles/workflow.md)).
If you review ring quality in the flags app
([`vignette("flags-app")`](https://tria-db.github.io/rxs2tria/articles/flags-app.md)),
extract the series after flagging.

### Which years are included

Each woodpiece yields one series, with its `woodpiece_label` as column
name. Two kinds of rings are removed before building the series:

- duplicate rings not selected as the preferred copy (`exclude_dupl`),
- rings flagged for exclusion (`exclude_issues`).

`NA` flags count as `FALSE`. Years of removed rings become `NA`
measurement values, and each series spans the full year range of the
`rwl` object, so gaps are explicit. If more than one ring per woodpiece
and year remains, the function aborts; check the `exclude_dupl` flags of
the reported woodpieces.

### Ring parameters

Any measurement column of the rings data (e.g. `mrw`, `eww`, `lww`,
`ra`) can be exported directly:

``` r

rwl_mrw <- extract_rwl(df_rings = QWA_data$rings, param = "mrw")
```

### Profile parameters

Aggregated cell parameters come from a sector profile (see
[`vignette("QWAprofile")`](https://tria-db.github.io/rxs2tria/articles/QWAprofile.md)).
Pass the profile as `prf_data` and select a `sector`; `param` must be a
mean or quantile measurement aggreagate column of the profile
(e.g. `la_mean`, `la_q90`):

``` r

prf_sector <- calculate_sector_profiles(
  QWA_data,
  n_sectors        = 5,
  sel_cell_params  = c("la", "cwtrad"),
  quant_probs      = c(0.1, 0.5, 0.9)
)

rwl_la <- extract_rwl(df_rings = QWA_data$rings, param = "la_q90",
                      prf_data = prf_sector, sector = 5)
```

The exclusion flags are always read from `df_rings`, so it is required
in both cases. The profile must be computed from the same `QWAdata`
object after
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md);
profile years without a rings row abort the extraction.

**Affected tissue:** By default, every value from a ring with
`exclude_issues` is removed by `extract_rwl`. However, in the flags app
([`vignette("flags-app")`](https://tria-db.github.io/rxs2tria/articles/flags-app.md))
you may have filled in the `affected_tissue` column to indicate that for
some `exclude_issues` rings, only the latewood or earlywood part of the
tissue may actually be affected. In that case, it is possilbe to
calculate the sector aggregates on the unaffected tissue cells only,
with `apply_exclusions = TRUE` and `tissue = "affected"`. Then
aggregates from rings whose issue only affects the earlywood or latewood
(`affected_tissue` `"ew"` or `"lw"`) are kept, using the values of the
unaffected tissue. In the rwl extraction, you can then relax the ring
exclusion criteria analogously to keep the values from aggregates of
unaffected cells in excluded rings.

``` r

prf_sector <- calculate_sector_profiles(
  QWA_data,
  n_sectors        = 5,
  sel_cell_params  = c("la", "cwtrad"),
  apply_exclusions = TRUE, # already filter out rings with exclude_* here
  tissue           = "affected" # exclude only cells from affected tissue
)

rwl_cwt <- extract_rwl(df_rings = QWA_data$rings, param = "cwtrad_mean",
                       prf_data = prf_sector, sector = 5,
                       tissue = "affected")
```

### Working with dplR

The result is a regular dplR `rwl` object, so the dplR functions apply
directly, e.g.:

``` r

dplR::rwl.report(rwl_mrw)
summary(rwl_mrw)

rwi <- dplR::detrend(rwl_mrw, method = "AgeDepSpline")
crn <- dplR::chron(rwi)
```

The values keep the units of the QWA data (e.g. µm for ring widths, µm²
for lumen area), and the column names are the `woodpiece_label`s. See
<https://opendendro.github.io/learningToLoveDplR> for more info.

------------------------------------------------------------------------

## Writing Tucson .rwl files

Tucson `.rwl` files are a widely supported exchange format for
ring-width series, read e.g. by COFECHA. The format imposes two
constraints on the `rwl` object:

- **Series IDs** of at most 6 characters (7-8 with `long.names = TRUE`),
  out of `A-Z`, `a-z` and `0-9`.
- **Values** stored as integers at a precision `prec` of `0.001` or
  `0.01`, with 5 digits of usable range.

### Series IDs

`woodpiece_label`s usually exceed the ID length.
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
would truncate them;
[`rename_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/rename_for_tucson.md)
instead derives short IDs from the data structure, using the first of
these variants that is unique and short enough for all series:

1.  site label + base ID (e.g. `YAM_LASI_122_a` → `YAM122a`),
2.  base ID only (`122a`),
3.  species code + base ID (`LASI122a`).

The base ID is the `woodpiece_label` without the site and species
prefixes, reduced to the allowed characters. The data structure columns
(`woodpiece_label`, `site_label`, optionally `species_code`) are taken
from a `QWAimages` object, e.g. `rxs_images` or `QWA_meta$images`. Pass
the same `long.names` value you will use when writing:

``` r

rwl_out <- rename_for_tucson(rwl_mrw, rxs_images, long.names = TRUE)
attr(rwl_out, "mapping")
```

The mapping from `woodpiece_label` to series ID is stored in the
`"mapping"` attribute. To preview the IDs without renaming, use
`make_short_series_ids(rxs_images, max_chars = 8)`. If no unique mapping
can be derived, the function aborts.

### Scaling

[`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md)
multiplies all values by a power of ten. ROXAS (AI) ring widths are
measured in µm, while
[`dplR::write.tucson`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
conventioally expects mm (written back to µm at precision 0.001 in the
actual Tucson file), so use `scaling = 0.001` for `mrw`, `eww` and
`lww`:

``` r

rwl_out <- scale_for_tucson(rwl_out, prec = 0.001, scaling = 0.001)
```

For other parameters, leave `scaling = NULL` to choose the largest
factor that keeps all values within the 5-digit range at the given
`prec`:

``` r

rwl_la_out <- rwl_la |>
  rename_for_tucson(rxs_images, long.names = TRUE) |>
  scale_for_tucson(prec = 0.001)
attr(rwl_la_out, "scaling")
```

A manual `scaling` that pushes values beyond the range aborts, with the
largest factor that fits as hint.

### Writing

Call
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
with the same `prec` and `long.names`:

``` r

dplR::write.tucson(rwl_out, fname = "mrw.rwl", prec = 0.001,
                   long.names = TRUE)
```

Note that
[`dplR::write.tucson()`](https://rdrr.io/pkg/dplR/man/write.tucson.html)
had a recent version change. Before version 1.8.0, missing and negative
values are written automatically to 0, while in the newer versions,
these values are replaced by the missing value marker 999, which may
lead to issues in COFECHA. use the parameter `fill.internal.NA = 0` to
establish the old behaviour, cf. <https://opendendro.org/r/#new-in-180>.

------------------------------------------------------------------------

## Writing TSAP import files

[`write_tsap()`](https://tria-db.github.io/rxs2tria/reference/write_tsap.md)
writes a tab-delimited text file for import into TSAP: a `Year` column
followed by one column per series, named by `woodpiece_label`. Values
are rounded to integers (since the standard import settings in TSAP do
not allow for decimal digits), and missing values are written as `0`.

TSAP typically expects ring widths in µm, so `mrw`, `eww` and `lww` can
be written as they are:

``` r

write_tsap(rwl_mrw, "mrw_tsap.txt")
```

Parameters on smaller scales (e.g. cell wall thickness, relative
measures) would lose most of their information when rounded. Scale them
first with
[`scale_for_tsap()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tsap.md),
which by default picks the largest power of ten keeping all values at
most 9999:

``` r

rwl_cwt_out <- scale_for_tsap(rwl_cwt)
attr(rwl_cwt_out, "scaling")
write_tsap(rwl_cwt_out, "cwttan_mean_tsap.txt")
```

[`write_tsap()`](https://tria-db.github.io/rxs2tria/reference/write_tsap.md)
warns if non-zero values round to `0`. As missing values are also
written as `0`, gaps from excluded rings cannot be told apart from
zero-width (missing) rings in the TSAP file.

------------------------------------------------------------------------

## What to keep in mind

- **Apply the preparation functions last.** The `"scaling"` and
  `"mapping"` attributes are kept by
  [`rename_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/rename_for_tucson.md)
  and
  [`scale_for_tucson()`](https://tria-db.github.io/rxs2tria/reference/scale_for_tucson.md)
  (so either order works), but dropped by most other operations
  (subsetting, arithmetic, dplR functions). An object that already
  carries the attribute is not scaled or renamed again.

- **Record the scaling factor and ID mapping.** Neither is stored in the
  written files. Divide the exported values by `scaling` to recover the
  original values (up to rounding). For the raw integers in a Tucson
  file, apply `* prec / scaling` instead; values re-read with
  [`dplR::read.tucson()`](https://rdrr.io/pkg/dplR/man/read.tucson.html)
  only need `/ scaling`. To keep the mapping, e.g.:

  ``` r

  write.csv(attr(rwl_out, "mapping"), "mrw_series_ids.csv",
            row.names = FALSE)
  ```

- **Long-format alternative.** For further analyses in R, use
  [`extract_ring_measures()`](https://tria-db.github.io/rxs2tria/reference/extract_ring_measures.md):
  it returns the ring measurements with the same rings removed, one row
  per woodpiece and year.
