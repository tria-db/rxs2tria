# Read and combine image exif data

Collect the exif data from all image files. Used preliminarily for data
generated with classic ROXAS, for ROXAS AI the image exif inormation is
already stored in the settings files.

## Usage

``` r
collect_image_info(files_images, batch_size = 50)
```

## Arguments

- files_images:

  Vector/list of image filenames

- batch_size:

  Nr of images to read exif data from at a time. Note that
  [`exifr::read_exif()`](https://rdrr.io/pkg/exifr/man/read_exif.html)
  is vectorized, so the only purpose for the batching is to generate a
  progress bar. Increase for more speed.

## Value

A dataframe containing the extracted data.
