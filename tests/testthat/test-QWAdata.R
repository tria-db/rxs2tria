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

test_that("complete_flags warns on images not in meta", {
  rings <- dplyr::select(rings_flags, "woodpiece_label":"mrw")
  cells <- data.frame(image_label = "WP1_SL1_IMG1", year = 2001L,
                      xpix = 1:3, ypix = 1:3, raddistr = 1:3)
  meta <- data.frame(image_label = "OTHER_IMG", spatial_resolution = 1)
  x <- new_QWAdata(cells = cells, rings = rings)

  expect_warning(x <- suppressMessages(complete_flags(x, meta)), "not in .meta")
  expect_true(x$rings$incomplete_ring[1])
})

# complete_measures ----
cells_meas <- data.frame(image_label = "WP1_SL1_IMG1", year = 2000L, xpix = 1:20, ypix = 1,
                         la = 100, cwa = 50, cwtrad = 2, cwttan = 3, cwtall = 2.5,
                         drad = 10, dtan = 12, rraddistr = seq(2.5, 97.5, by = 5),
                         rtsr = rep(c(0.5, 2), each = 10)) # EW-like, then LW-like
rings_meas <- data.frame(woodpiece_label = "WP1", slide_label = "WP1_SL1",
                         image_label = "WP1_SL1_IMG1", year = 2000L, mrw = 100)
x_meas <- new_QWAdata(cells = cells_meas, rings = rings_meas)

test_that("complete_measures adds the derived and estimate columns of the schemas", {
  added <- \(x) setdiff(c(names(x$cells), names(x$rings)), c(names(cells_meas), names(rings_meas)))
  full <- suppressMessages(complete_measures(x_meas))
  # every derived/estimate column of the schemas has a calculation
  expect_setequal(added(full), c(schema_cols("cells", c("derived", "estimate")),
                                 schema_cols("rings", c("derived", "estimate"))))
  expect_setequal(unique(full$cells$ew_lw), c("ew", "lw"))

  # removed columns are recalculated, the others kept
  x <- full
  x$cells$sector100 <- NULL
  x$rings$eww <- NULL
  x <- suppressMessages(complete_measures(x))
  expect_equal(x$cells, full$cells)
  expect_equal(x$rings, full$rings)
})

# QWAdata helper ----
cells_raw <- data.frame(image_label = "WP1_SL1_IMG1", year = c(2000, 2000, 2002),
                        xpix = c(1, 2, 3), ypix = c(1, 2, 3), cwttan = 1)
rings_raw <- data.frame(woodpiece_label = "WP1", slide_label = "WP1_SL1",
                        image_label = "WP1_SL1_IMG1", year = c(2000, 2002), my_col = "x")
# warnings of QWAdata(), messages muted
qwa_warnings <- function(...) capture_warnings(suppressMessages(QWAdata(...)))

test_that("QWAdata aligns to the schema, keeps extra columns and does not complete rings", {
  w <- qwa_warnings(cells_raw, rings_raw)
  x <- suppressWarnings(suppressMessages(QWAdata(cells_raw, rings_raw)))

  expect_type(x$rings$year, "integer")
  expect_type(x$cells$xpix, "integer")
  expect_equal(names(x$rings)[ncol(x$rings)], "my_col")
  expect_false("cno" %in% names(x$rings))
  expect_equal(x$rings$year, c(2000L, 2002L))
  expect_match(w, "Gaps", all = FALSE)
  expect_no_match(paste(w, collapse = ""), "cno|no row|duplicate")
})

test_that("QWAdata aborts on missing key columns", {
  expect_error(suppressMessages(QWAdata(rings = dplyr::select(rings_raw, -"slide_label"))),
               "missing key column")
})

test_that("QWAdata warns on duplicate cell keys and cells vs rings issues", {
  expect_match(qwa_warnings(cells = cells_raw[c(1, 1:3), ]), "duplicate", all = FALSE)
  expect_match(qwa_warnings(cells_raw, rings_raw[1, ]), "no row for 1 year", all = FALSE)

  expect_match(qwa_warnings(cells_raw, dplyr::mutate(rings_raw, cno = c(2L, 5L))),
               "cno differs", all = FALSE)
  expect_no_match(paste(qwa_warnings(cells_raw, dplyr::mutate(rings_raw, cno = c(2L, 1L))),
                        collapse = ""), "cno differs")

  img2 <- dplyr::mutate(rings_raw[1, ], image_label = "WP1_SL1_IMG2")
  expect_match(qwa_warnings(cells_raw, dplyr::bind_rows(rings_raw, img2)),
               "rings.*but not in.*cells", all = FALSE)
  expect_match(qwa_warnings(cells_raw, img2), "no images in common", all = FALSE)
})

# check_QWAdata ----
cells_ok <- dplyr::mutate(cells_raw, year = c(2000L, 2000L, 2001L))
rings_ok <- data.frame(woodpiece_label = "WP1", slide_label = "WP1_SL1",
                       image_label = "WP1_SL1_IMG1", year = 2000:2001, cno = 2:1)
x_ok <- new_QWAdata(cells = cells_ok, rings = rings_ok)

test_that("complete_rings labels images without rings rows from meta, check_QWAdata warns", {
  cells <- data.frame(image_label = c("WP1_SL1_IMG1", "WP1_SL2_IMG2"), year = 2000L, xpix = 1, ypix = 1)
  rings <- data.frame(woodpiece_label = "WP1", slide_label = "WP1_SL1",
                      image_label = "WP1_SL1_IMG1", year = 2000L, mrw = 100)
  meta <- data.frame(image_label = c("WP1_SL1_IMG1", "WP1_SL2_IMG2"), woodpiece_label = "WP1",
                     slide_label = c("WP1_SL1", "WP1_SL2"))
  expect_message(r <- complete_rings(new_QWAdata(cells, rings), meta), "without rings rows")
  expect_equal(r$slide_label[r$image_label == "WP1_SL2_IMG2"], "WP1_SL2")
  expect_equal(r$cno[r$image_label == "WP1_SL2_IMG2"], 1L)

  x <- new_QWAdata(cells, r)
  expect_match(capture_warnings(suppressMessages(check_QWAdata(x))),
               "1 image without ring measurements", all = FALSE)
})

test_that("check_QWAdata returns TRUE without issues", {
  expect_no_warning(res <- suppressMessages(check_QWAdata(x_ok)))
  expect_true(res)
})

test_that("check_QWAdata warns, returns FALSE and aborts with strict", {
  bad_schema <- new_QWAdata(cells = dplyr::mutate(cells_ok, la = -1), rings = rings_ok)
  expect_warning(res <- suppressMessages(check_QWAdata(bad_schema)), "failed validation")
  expect_false(res)
  expect_error(suppressMessages(check_QWAdata(bad_schema, strict = TRUE)), "failed validation")

  chk_warnings <- function(x) capture_warnings(suppressMessages(check_QWAdata(x)))
  dupl <- new_QWAdata(cells = cells_ok[c(1, 1:3), ], rings = rings_ok)
  expect_match(chk_warnings(dupl), "duplicate", all = FALSE)

  stale <- new_QWAdata(cells = cells_ok[1:2, ], rings = rings_ok)
  expect_match(chk_warnings(stale), "cno.*differs")

  gaps <- new_QWAdata(cells = NULL, rings = dplyr::mutate(rings_ok, year = c(2000L, 2002L)))
  expect_match(chk_warnings(gaps), "Gaps")

  flags_na <- new_QWAdata(cells = NULL, rings = dplyr::mutate(rings_ok, incomplete_ring = c(FALSE, NA)))
  expect_match(chk_warnings(flags_na), "missing values in ring flag", all = FALSE)
})

test_that("NA issue flags do not make exclude_dupl NA", {
  rings <- dplyr::mutate(rings_flags, woodpiece_label = "WP1", year = 2001,
                         image_label = paste0("WP1_SL1_IMG", 1:3),
                         incomplete_ring = c(NA, FALSE, FALSE))
  res <- flag_duplicate_rings(rings)
  expect_false(anyNA(res$exclude_dupl))
  expect_equal(sum(!res$exclude_dupl), 1)
})

test_that("check_QWAdata informs on extra and missing optional columns", {
  x <- new_QWAdata(cells = cells_ok, rings = dplyr::mutate(rings_ok, my_col = "x"))
  msgs <- capture_messages(check_QWAdata(x))
  expect_match(msgs, "extra columns", all = FALSE)
  expect_match(msgs, "missing optional columns", all = FALSE)
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

test_that("check_outmost_year warns after outmost_year + 1, informs if missing", {
  meta <- data.frame(image_label = c("WP1_SL1_IMG1", "WP1_SL1_IMG2"),
                     outmost_year = c(2001, NA))
  # incomplete outermost ring allowed; images not in meta skipped
  expect_true(check_outmost_year(rings_dates(2000:2002), meta))
  expect_true(check_outmost_year(rings_dates(2000:2005, img = "OTHER"), meta))

  expect_warning(res <- check_outmost_year(rings_dates(2000:2003), meta), "after")
  expect_false(res)
  expect_error(check_outmost_year(rings_dates(2000:2003), meta, strict = TRUE), "after")

  expect_message(res <- check_outmost_year(rings_dates(2000:2002, img = "WP1_SL1_IMG2"), meta),
                 "missing for 1 image")
  expect_true(res)
  expect_message(check_outmost_year(rings_dates(2000:2002), meta["image_label"]),
                 "missing for 1 image")
})

test_that("check_cell_years warns on future years, check_cwt only informs", {
  future <- as.numeric(format(Sys.Date(), "%Y")) + 1
  cells <- data.frame(image_label = "WP1_SL1_IMG1", year = future, cwttan = NA_real_)
  expect_warning(res <- check_cell_years(cells), "future")
  expect_false(res)
  expect_message(res <- check_cwt(cells), "cwttan")
  expect_true(res)
})
