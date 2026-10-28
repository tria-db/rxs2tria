test_that("validate_QWAprofile aborts on missing, NA and duplicate keys", {
  sector <- data.frame(image_label = "IMG1", year = 2000L, sector_n = 1:2, la_mean = 1)
  expect_s3_class(QWAprofile(sector, "sector"), "QWAprofile")
  expect_error(QWAprofile(dplyr::mutate(sector, sector_n = factor(sector_n)), "sector"), "wrong type")
  expect_error(QWAprofile(dplyr::mutate(sector, sector_n = sector_n + 0.5), "sector"), "wrong type")
  expect_error(QWAprofile(sector[c(1, 1), ], "sector"), "duplicate")
  expect_error(QWAprofile(dplyr::mutate(sector, year = NA_integer_), "sector"), "missing values")
  expect_error(QWAprofile(sector, "band"), "missing key column")

  band <- data.frame(image_label = "IMG1", year = 2000L, start = c(0, 100), end = c(100, 100),
                     la_mean = 1)
  expect_error(QWAprofile(band, "band"), "1 band with")
})

test_that("check_QWAprofile passes a valid profile; counts and ring context are not measurements", {
  sector <- data.frame(image_label = "IMG1", year = 2000L, sector_n = 1:2, la_N = 3L, la_mean = 1)
  expect_true(suppressMessages(check_QWAprofile(sector |> QWAprofile("sector"))))

  no_meas <- data.frame(image_label = "IMG1", year = 2000L, start = 0, end = 50, mrw = 100,
                        eww = 60, ew_band = TRUE, la_N = 3L)
  expect_warning(QWAprofile(no_meas, "band"), "no measurement columns")
})

test_that("profile data issues warn in QWAprofile() and fail check_QWAprofile()", {
  sector <- data.frame(image_label = "IMG1", year = 2000L, sector_n = 0:1, la_mean = 1)
  band <- data.frame(image_label = "IMG1", year = 2000L, start = c(0, 10), end = c(50, 70),
                     la_mean = 1)
  all_na <- data.frame(image_label = "IMG1", year = 2000L, sector_n = 1:2, la_N = 0L,
                       la_mean = NA_real_)
  cases <- list(
    list(sector, "sector", "1 row with"),
    list(band, "band", "different widths"),
    list(all_na, "sector", "all measurement values are missing")
  )
  for (case in cases) {
    expect_warning(prf <- QWAprofile(case[[1]], case[[2]]), case[[3]])
    expect_warning(res <- suppressMessages(check_QWAprofile(prf)), case[[3]])
    expect_false(res)
    expect_error(check_QWAprofile(prf, strict = TRUE), case[[3]])
  }
})
