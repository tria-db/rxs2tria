# complete_flags: exclude_issues ----
# all automatic flags except exclude_issues present, so only that is derived
rings_flags <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~cno, ~mrw, ~incomplete_ring, ~missing_ring, ~duplicate_ring, ~exclude_dupl,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 50, 500, TRUE,  FALSE, FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002,  0,   0, FALSE, TRUE,  FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 60, 600, FALSE, FALSE, FALSE, FALSE,
)
x_flags <- new_QWAdata(cells = NULL, rings = rings_flags)

test_that("exclude_issues is initialised from incomplete_ring only", {
  x <- suppressMessages(complete_flags(x_flags, meta = NULL))

  expect_equal(x$rings$exclude_issues, c(TRUE, FALSE, FALSE))
})

test_that("deprecated exclude_mode warns and still applies", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  expect_warning(
    x <- suppressMessages(complete_flags(x_flags, meta = NULL, exclude_mode = "either")),
    "deprecated")
  expect_equal(x$rings$exclude_issues, c(TRUE, TRUE, FALSE))

  suppressMessages(
    expect_warning(check_QWAdata(x, exclude_mode = "either"), "deprecated"))
})

test_that("complete_flags warns on missing spatial_resolution", {
  rings <- dplyr::select(rings_flags, "woodpiece_label":"mrw")
  cells <- data.frame(image_label = "WP1_SL1_IMG1", year = 2001L,
                      xpix = 1:3, ypix = 1:3, raddistr = 1:3)
  meta <- data.frame(image_label = c("WP1_SL1_IMG1", "OTHER_IMG"), spatial_resolution = NA_real_)
  x <- new_QWAdata(cells = cells, rings = rings)

  expect_warning(x <- suppressMessages(complete_flags(x, meta)), "innermost rings")
  expect_true(x$rings$incomplete_ring[1])
})

# dating checks ----
rings_dates <- function(years, img = "WP1_SL1_IMG1") {
  data.frame(image_label = img, year = years)
}

test_that("validate_QWAdata aborts on undated and duplicated ring years", {
  rings <- data.frame(woodpiece_label = "WP1", slide_label = "WP1_SL1",
                      image_label = "WP1_SL1_IMG1", year = c(2000, 2001))
  expect_no_error(validate_QWAdata(new_QWAdata(rings = rings)))
  expect_error(validate_QWAdata(new_QWAdata()), "at least one")
  expect_error(validate_QWAdata(new_QWAdata(rings = dplyr::mutate(rings, year = c(2000, NA)))),
               "missing values")
  expect_error(validate_QWAdata(new_QWAdata(rings = dplyr::mutate(rings, year = 2000))),
               "duplicate")
  expect_error(validate_QWAdata(new_QWAdata(rings = dplyr::select(rings, -"slide_label"))),
               "missing key column")

  # duplicate cell keys are no class invariant
  cells <- data.frame(image_label = "WP1_SL1_IMG1", year = 2000, xpix = 1, ypix = 1)
  expect_no_error(validate_QWAdata(new_QWAdata(cells = cells[c(1, 1), ])))
})

test_that("check_ring_years warns on gaps and future years, aborts with strict", {
  expect_true(check_ring_years(rings_dates(2000:2002)))

  expect_warning(res <- check_ring_years(rings_dates(c(2000, 2002))), "Gaps")
  expect_false(res)
  expect_error(check_ring_years(rings_dates(c(2000, 2002)), strict = TRUE), "Gaps")

  future <- as.numeric(format(Sys.Date(), "%Y")) + 1
  expect_warning(check_ring_years(rings_dates(c(future - 1, future))), "future")
})

test_that("check_cell_years warns on future years, check_cwt only informs", {
  future <- as.numeric(format(Sys.Date(), "%Y")) + 1
  cells <- data.frame(image_label = "WP1_SL1_IMG1", year = future, cwttan = NA_real_)
  expect_warning(res <- check_cell_years(cells), "future")
  expect_false(res)
  expect_message(res <- check_cwt(cells), "cwttan")
  expect_true(res)
})
