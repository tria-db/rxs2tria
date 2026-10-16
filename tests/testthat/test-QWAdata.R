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
