# Check the consistency of QWA objects

Checks that the objects of a dataset match each other, as the last check
before a submission
([`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md),
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md)
and
[`check_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/check_QWAprofile.md)
check each object on its own). Aborts if fewer than two objects are
given or an object is not valid. Warns (or, with `strict = TRUE`,
aborts) on:

- `meta` and `data`: images in `meta` but not in `data$cells` or
  `data$rings`, and vice versa; `woodpiece_label` or `slide_label` in
  `data$rings` differing from `meta`; ring years after `outmost_year` +
  1 (an incomplete outermost ring). Images without `outmost_year` are
  reported as not checked.

- `data` and `profile`: years of an image in `profile` without a row in
  `data$rings` (the flags app and
  [`extract_rwl()`](https://tria-db.github.io/rxs2tria/reference/extract_rwl.md)
  refuse such profiles).

- `meta` and `profile`, if `data` is not given: images in `profile` but
  not in `meta`.

## Usage

``` r
check_consistency(meta = NULL, data = NULL, profile = NULL, strict = FALSE)
```

## Arguments

- meta:

  A
  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  or
  [QWAmetadata](https://tria-db.github.io/rxs2tria/reference/QWAmetadata.md)
  object (its `images` component is used).

- data:

  A [QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
  object.

- profile:

  A
  [QWAprofile](https://tria-db.github.io/rxs2tria/reference/QWAprofile.md)
  object.

- strict:

  If `TRUE`, abort on the first issue instead of warning.

## Value

`TRUE` if no issue was found, `FALSE` otherwise (invisibly).

## See also

[`check_QWAimages()`](https://tria-db.github.io/rxs2tria/reference/check_QWAimages.md),
[`check_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAmetadata.md),
[`check_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/check_QWAdata.md),
[`check_QWAprofile()`](https://tria-db.github.io/rxs2tria/reference/check_QWAprofile.md)
