test_that("validate_QWAprofile aborts on missing, NA and duplicate keys", {
  sector <- data.frame(image_label = "IMG1", year = 2000L, sector_n = factor(1:2), la_mean = 1)
  expect_s3_class(QWAprofile(sector, "sector"), "QWAprofile")
  expect_error(QWAprofile(sector[c(1, 1), ], "sector"), "duplicate")
  expect_error(QWAprofile(dplyr::mutate(sector, year = NA_integer_), "sector"), "missing values")
  expect_error(QWAprofile(sector, "band"), "missing key column")

  band <- data.frame(image_label = "IMG1", year = 2000L, start = c(0, 100), end = c(100, 100),
                     la_mean = 1)
  expect_error(QWAprofile(band, "band"), "1 band with")
})
