# Remove outliers in cells and rings data

ROXAS does some automatic outlier detection and replaces the found
outliers in cell (and ring?) measures by their negative value. This
function replaces these 'negatives' with NA.

## Usage

``` r
remove_outliers(QWA_data)
```

## Arguments

- QWA_data:

  a list containing the cells and rings dataframes

## Value

QWA_data with the 'negative' outliers replaced by NAs
