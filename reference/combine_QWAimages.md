# Combine two QWAimages objects

Combines the images of two
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
objects, e.g. from separate input folders, into one. Both objects must
have the same `roxas_version` and distinct `image_label` values. The
result is aligned and validated by
[`QWAimages()`](https://tria-db.github.io/rxs2tria/reference/QWAimages.md).

## Usage

``` r
combine_QWAimages(x, y)
```

## Arguments

- x, y:

  [QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
  objects

## Value

A combined
[QWAimages](https://tria-db.github.io/rxs2tria/reference/QWAimages.md)
object

## See also

[`combine_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAdata.md),
[`combine_QWAmetadata()`](https://tria-db.github.io/rxs2tria/reference/combine_QWAmetadata.md)
