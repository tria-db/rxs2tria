# Fixtures ----
# - IMG1 2001: kept
# - IMG1 2002: excluded, affected_tissue "ew"
# - IMG1 2003: not selected duplicate (IMG2 2003 is kept)
# - IMG2 2004: excluded, affected_tissue NA
# - IMG2 2005: NA exclude_issues flag, counts as not excluded
# - IMG2 2006: excluded, affected_tissue "lw"
rings_prf <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~mrw, ~eww, ~exclude_dupl, ~exclude_issues, ~affected_tissue,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 100, 50, FALSE, FALSE, NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002, 100, 50, FALSE, TRUE,  "ew",
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 100, 50, TRUE,  FALSE, NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2003, 100, 50, FALSE, FALSE, NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2004, 100, 50, FALSE, TRUE,  NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2005, 100, 50, FALSE, NA,    NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2006, 100, 50, FALSE, TRUE,  "lw",
)
# four cells per ring: two ew cells in sector 1, two lw cells in sector 2
cells_prf <- rings_prf |>
  dplyr::select(image_label, year) |>
  tidyr::crossing(tibble::tibble(rraddistr = c(10, 40, 60, 90),
                                 ew_lw = c("ew", "ew", "lw", "lw"))) |>
  dplyr::mutate(xpix = 1, ypix = 1, la = rraddistr)
x_prf <- new_QWAdata(cells = cells_prf, rings = rings_prf)

# single ring with la equal to rraddistr
one_ring <- function(rraddistr, mrw = 100, eww = 50) {
  new_QWAdata(
    cells = tibble::tibble(image_label = "IMG", year = 2001, xpix = 1, ypix = 1,
                           rraddistr = rraddistr, la = rraddistr),
    rings = tibble::tibble(woodpiece_label = "WP", slide_label = "SL",
                           image_label = "IMG", year = 2001, mrw = mrw, eww = eww))
}

sector_prf <- function(...) suppressMessages(calculate_sector_profiles(...))
band_prf <- function(...) suppressMessages(calculate_band_profiles(...))

prf_rings <- function(prf) {
  tibble::tibble(image_label = prf$image_label, year = prf$year) |>
    dplyr::distinct() |>
    dplyr::arrange(image_label, year)
}

# Sector profiles ----
test_that("sector profiles aggregate all rings by default", {
  prf <- sector_prf(x_prf, 2, "la")

  expect_s3_class(prf, "QWAprofile")
  expect_equal(attr(prf, "profile_type"), "sector")
  expect_equal(prf$sector_n, rep(c(1, 2), 7))
  expect_equal(prf$la_N, rep(2, 14))
  expect_equal(prf$la_mean, rep(c(25, 75), 7))
})

test_that("sector boundaries are right-closed and tolerate rraddistr up to 101", {
  prf <- sector_prf(one_ring(c(0, 50, 50.5, 100, 100.5, 102, NA)), 2, "la")

  expect_equal(prf$la_N, c(2, 3))
  expect_equal(prf$la_mean, c(25, (50.5 + 100 + 100.5) / 3))
})

test_that("quantile columns are named by percentage per parameter", {
  x <- one_ring(c(10, 20, 30, 40))
  x$cells$cwttan <- x$cells$la / 10
  prf <- sector_prf(x, 1, c("la", "cwttan"), quant_probs = c(0.05, 0.257, 1))

  expect_equal(prf$la_q05, 11.5)
  expect_equal(prf$la_q25.7, 17.71)
  expect_equal(prf$la_q100, 40)
  expect_equal(prf$cwttan_q05, 1.15)
})

test_that("sel_cell_params = 'all' uses only cell parameter columns", {
  prf <- sector_prf(x_prf, 2, "all")

  expect_equal(grep("_mean$", names(prf), value = TRUE), "la_mean")
})

# Exclusions (works the same for bands and sectors) ----
test_that("sector profiles with exclusions drop excluded rings", {
  prf <- sector_prf(x_prf, 2, "la", apply_exclusions = TRUE)

  expect_equal(prf_rings(prf),
               tibble::tibble(image_label = c("WP1_SL1_IMG1", rep("WP1_SL1_IMG2", 2)),
                              year = c(2001, 2003, 2005)))
})

test_that("tissue = 'affected' drops only the affected tissue", {
  prf <- sector_prf(x_prf, 2, "la", apply_exclusions = TRUE, tissue = "affected")

  # ring 2002 has only 'lw' sector 2
  ring_2002 <- prf |> dplyr::filter(image_label == "WP1_SL1_IMG1", year == 2002)
  expect_equal(ring_2002$sector_n, 2)
  expect_equal(ring_2002$la_mean, 75)
  # ring 2006 has only 'ew' sector 1
  ring_2006 <- prf |> dplyr::filter(image_label == "WP1_SL1_IMG2", year == 2006)
  expect_equal(ring_2006$sector_n, 1)
  expect_equal(ring_2006$la_mean, 25)
  # duplicate and NA affected_tissue: whole ring still dropped
  expect_false(any(prf$image_label == "WP1_SL1_IMG1" & prf$year == 2003))
  expect_false(any(prf$image_label == "WP1_SL1_IMG2" & prf$year == 2004))
})

# Band profiles ----
test_that("band profiles aggregate cells per band and flag ew bands", {
  prf <- band_prf(one_ring(c(10, 40, 60, 90)), 50, 50, "la")

  expect_equal(attr(prf, "profile_type"), "band")
  expect_equal(prf$start, c(0, 50))
  expect_equal(prf$end, c(50, 100))
  expect_equal(prf$la_N, c(2, 2))
  expect_equal(prf$la_mean, c(25, 75))
  expect_equal(prf$ew_band, c(TRUE, FALSE))
})

test_that("band_rebound shifts the last band to end at mrw", {
  # cells at 6, 24 and 54 microns
  x <- one_ring(c(10, 40, 90), mrw = 60)

  prf <- band_prf(x, 50, 50, "la")
  expect_equal(prf$start, c(0, 10))
  expect_equal(prf$la_N, c(2, 2))

  prf <- band_prf(x, 50, 50, "la", band_rebound = FALSE)
  expect_equal(prf$start, c(0, 50))
  expect_equal(prf$la_N, c(2, 1))
})

test_that("overlapping bands count cells in each band they fall in", {
  prf <- band_prf(one_ring(c(10, 40, 60, 90)), 50, 25, "la")

  expect_equal(prf$start, c(0, 25, 50))
  expect_equal(prf$la_N, c(2, 2, 2))
  expect_equal(prf$la_mean, c(25, 50, 75))
})

test_that("rings with mrw NA or not exceeding bandwidth get no bands", {
  x <- new_QWAdata(
    cells = tibble::tibble(image_label = "IMG", year = 2001:2003, xpix = 1, ypix = 1,
                           rraddistr = 10, la = 10),
    rings = tibble::tibble(woodpiece_label = "WP", slide_label = "SL",
                           image_label = "IMG", year = 2001:2003, mrw = c(100, 50, NA)))

  expect_equal(unique(band_prf(x, 50, 50, "la")$year), 2001)
})
