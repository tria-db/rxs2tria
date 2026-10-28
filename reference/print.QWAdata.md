# Print a QWAdata object

Displays a compact overview of a
[QWAdata](https://tria-db.github.io/rxs2tria/reference/QWAdata.md)
object: which components are present, the spatial and temporal coverage
of the data, and (if the flag columns are present, see
[`complete_QWAdata()`](https://tria-db.github.io/rxs2tria/reference/complete_QWAdata.md))
a summary of the automatic ring flags.

## Usage

``` r
# S3 method for class 'QWAdata'
print(x, ...)

# S3 method for class 'QWAdata'
summary(object, ...)
```

## Arguments

- x:

  A `QWAdata` object.

- ...:

  Further arguments (currently unused).

- object:

  A `QWAdata` object.

## Value

`x`, invisibly.

## See also

[`QWAdata()`](https://tria-db.github.io/rxs2tria/reference/QWAdata.md),
`summary.QWAdata()`
