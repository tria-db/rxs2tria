rings <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year,
  "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", 2000,
  "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", 2001,
  "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2", 2001,
  "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2", 2002,
)
cells <- rings |>
  dplyr::select("image_label", "year") |>
  dplyr::mutate(xpix = 1, ypix = 1)
data <- new_QWAdata(cells = cells, rings = rings)

make_meta <- function(outmost_year = c(2001, 2002), n = 2) {
  df <- rings |>
    dplyr::distinct(woodpiece_label, slide_label, image_label) |>
    dplyr::mutate(site_label = "S1", tree_label = "S1_T1", software = "roxas",
                  outmost_year = outmost_year) |>
    utils::head(n)
  new_QWAimages(as.data.frame(df), "roxas")
}

check_quiet <- function(...) suppressMessages(check_consistency(...))

test_that("check_consistency needs two valid objects", {
  expect_error(check_consistency(meta = make_meta()), "at least two")
  expect_error(check_consistency(meta = make_meta(), data = list()))
})

test_that("check_consistency passes on consistent meta and data", {
  expect_true(check_quiet(meta = make_meta(), data = data))
  # QWAmetadata: uses $images
  expect_true(check_quiet(meta = new_QWAmetadata(images = make_meta()), data = data))
  # data without cells
  expect_true(check_quiet(meta = make_meta(), data = new_QWAdata(rings = rings)))
})

test_that("check_consistency warns on images in meta or data only", {
  data_rings <- new_QWAdata(rings = rings)
  expect_warning(res <- check_quiet(meta = make_meta(n = 1), data = data_rings),
                 "in rings but not in")
  expect_false(res)
  expect_error(check_quiet(meta = make_meta(n = 1), data = data, strict = TRUE),
               "in cells but not in")

  rings_1 <- dplyr::filter(rings, image_label == "S1_T1_WP1_SL1_IMG1")
  expect_warning(check_quiet(meta = make_meta(), data = new_QWAdata(rings = rings_1)),
                 "but not in rings")
})

test_that("check_consistency warns on rings labels differing from meta", {
  rings_wp <- dplyr::mutate(rings, woodpiece_label = "S1_T1_WP2")
  expect_warning(res <- check_quiet(meta = make_meta(), data = new_QWAdata(rings = rings_wp)),
                 "differ from")
  expect_false(res)
})

test_that("check_consistency warns on profile years without rings rows", {
  prf <- new_QWAprofile(
    data.frame(image_label = "S1_T1_WP1_SL1_IMG1", year = c(2000L, 2001L),
               sector_n = 1L, la_mean = 1),
    "sector"
  )
  expect_true(check_quiet(data = data, profile = prf))
  expect_true(check_quiet(meta = make_meta(), data = data, profile = prf))

  prf_2 <- prf
  prf_2$year[2] <- 2005L
  expect_warning(res <- check_quiet(data = data, profile = prf_2), "with a profile")
  expect_false(res)
  # data without rings: all profile years missing
  expect_warning(check_quiet(data = new_QWAdata(cells = cells), profile = prf),
                 "no row for 2 years")
})

test_that("check_consistency warns on profile images not in meta without data", {
  prf <- new_QWAprofile(
    data.frame(image_label = "S1_T1_WP1_SL1_IMG1", year = 2000L, sector_n = 1L, la_mean = 1),
    "sector"
  )
  expect_true(check_quiet(meta = make_meta(), profile = prf))
  prf$image_label <- "OTHER"
  expect_warning(res <- check_quiet(meta = make_meta(n = 1), profile = prf),
                 "profile. but not in .meta")
  expect_false(res)
})

test_that("check_consistency checks ring years against outmost_year", {
  expect_warning(res <- check_quiet(meta = make_meta(c(1999, 2002)), data = data),
                 "after")
  expect_false(res)
  expect_message(res <- check_consistency(meta = make_meta(c(NA, 2002)), data = data),
                 "missing for 1 image")
  expect_true(res)
})
