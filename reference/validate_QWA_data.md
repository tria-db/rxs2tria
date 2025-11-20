# Validate the raw QWA data

Initial checks to ensure the quality of the raw QWA data. This function
checks for the following issues:

- undated images, i.e. YEAR is NA or in the future (raises error)

- images without cell wall thickness estimates (raises warning, since
  CWT is required for conifers but may not be available in the case of
  angiosperms)

## Usage

``` r
validate_QWA_data(QWA_data, df_meta, verbose_flags = FALSE)
```

## Arguments

- QWA_data:

  a list containing the cells and rings dataframes

- df_meta:

  a dataframe containing the metadata for the images (spatial_resolution
  required for the incomplete innermost ring check)

## Value

validated QWA_data (no changes to cells df, but added minimum required
ring flag columns to rings df).

## Details

Next, the function identifies the rings with the following issues:

- incomplete rings

- missing rings

- duplicate rings

Here, by **incomplete** rings we mean those at the inner (pith) and
outer (bark) boundaries of an images, which are cut-off by the image or
slide border. For these rings, some cells are usually recognized but the
MRW can NOT (outer) or NOT ACCURATELY (inner) be estimated. In some
cases, the user may have manually deleted the incomplete rings within
ROXAS already, so it is not a priori clear that all inner- and outermost
rings per image are incomplete.

NOTE: Because ROXAS uses the outer ring boundary to estimate MRW, the
innermost ring generally has an MRW estimate (that is not based on the
true ring boundary but rather the image border), while the outermost
ring has no MRW value (except if it is actually complete either because
it is at the at the bark or because the user removed the incomplete ring
manually in ROXAS). Therefore, we perform an additional check on the
border shape and position to check if an innermost ring is incomplete,
while the outermost ring is flagged as incomplete if and only if it has
no MRW.

**Missing** rings are for years that have no discernible ring in the
image, but have been manually added in ROXAS during cross-dating,
leading to an entry in the rings data but no corresponding no entries
(cells) in the cells data. This is usually the case with wedging rings.

**Duplicate** rings are those that are present in multiple images due to
them overlapping. All years which have cells in more than one image are
flagged and ranked by their number of cells. The (complete) year with
the highest number of cells for each overlap is the one that would then
usually be selected for further analysis when building chronologies.
