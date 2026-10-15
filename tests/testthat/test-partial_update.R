# Fixtures ----
# Existing QWAdata (completed, with edits from the flags app):
# - WP1 2003: IMG1/IMG2 overlap; auto would select IMG2 (higher cno), but the
#   user manually selected IMG1 instead
# - IMG1 2001: incomplete ring, auto-excluded
# - IMG1 2002: manually excluded (decay), with comment
rings_old <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~cno, ~incomplete_ring, ~missing_ring, ~duplicate_ring, ~exclude_dupl, ~exclude_issues, ~affected_tissue, ~comment,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 50, TRUE,  FALSE, FALSE, FALSE, TRUE,  NA,      NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002, 60, FALSE, FALSE, FALSE, FALSE, TRUE,  "decay", "crack",
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 70, FALSE, FALSE, TRUE,  FALSE, FALSE, NA,      NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2003, 80, FALSE, FALSE, TRUE,  TRUE,  FALSE, NA,      NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2004, 90, FALSE, FALSE, FALSE, FALSE, FALSE, NA,      NA,
  "WP2", "WP2_SL1", "WP2_SL1_IMG3", 2001, 40, FALSE, FALSE, FALSE, FALSE, FALSE, NA,      NA,
  "WP2", "WP2_SL1", "WP2_SL1_IMG3", 2002, 45, FALSE, FALSE, FALSE, FALSE, FALSE, NA,      NA,
)
cells_old <- rings_old |>
  dplyr::distinct(image_label, year) |>
  dplyr::mutate(la = 1)
x_old <- new_QWAdata(cells = cells_old, rings = rings_old)

# Current raw data on disk (as returned by the mocked collect_raw_data):
# - IMG1 re-analysed: 2001 no longer incomplete, 2003 cno still below IMG2
# - IMG4 is a new image of WP1, overlapping IMG2 in 2004 with a higher cno
rings_disk <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~cno, ~incomplete_ring, ~missing_ring,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 55,  FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002, 65,  FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 75,  FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2003, 80,  FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2004, 90,  FALSE, FALSE,
  "WP1", "WP1_SL2", "WP1_SL2_IMG4", 2004, 100, FALSE, FALSE,
  "WP1", "WP1_SL2", "WP1_SL2_IMG4", 2005, 110, FALSE, FALSE,
)
cells_disk <- rings_disk |>
  dplyr::distinct(image_label, year) |>
  dplyr::mutate(la = 2)

make_meta <- function(labels) {
  df <- dplyr::bind_rows(rings_old, rings_disk) |>
    dplyr::distinct(woodpiece_label, slide_label, image_label) |>
    dplyr::filter(image_label %in% labels)
  new_QWAimages(as.data.frame(df), "roxas")
}

# replace file reading and flag/measure calculation with simple stand-ins,
# so the tests target the update logic only
local_update_mocks <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    collect_raw_data = function(df_meta, roxas_version = NULL) {
      new_QWAdata(
        cells = dplyr::filter(cells_disk, image_label %in% df_meta$image_label),
        rings = dplyr::filter(rings_disk, image_label %in% df_meta$image_label))
    },
    complete_measures = function(QWA_data, only = NULL) QWA_data,
    complete_flags = function(x, meta, exclude_mode = NULL) {
      mode <- resolve_exclude_mode(exclude_mode)
      x$rings <- x$rings |>
        dplyr::mutate(exclude_issues = switch(mode,
          "either" = incomplete_ring | missing_ring,
          "incomplete_only" = incomplete_ring)) |>
        flag_duplicate_rings()
      x
    },
    .env = env
  )
}

get_ring <- function(x, img, yr) {
  x$rings |> dplyr::filter(image_label == img, year == yr)
}

# Tests ----
test_that("without re-read, new images are added and missing images removed", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG1", "WP1_SL1_IMG2", "WP1_SL2_IMG4"))

  expect_message(x <- update_QWAdata(x_old, meta), "Adding 1 new image")

  expect_setequal(unique(x$rings$image_label), meta$image_label)
  expect_setequal(unique(x$cells$image_label), meta$image_label)
  # existing images are not re-read
  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2001)$cno, 50)
  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2002)$comment, "crack")
  # new image: no manual values
  expect_false(any(get_ring(x, "WP1_SL2_IMG4", 2004)$exclude_issues))
  expect_true(is.na(get_ring(x, "WP1_SL2_IMG4", 2004)$comment))
})

test_that("duplicate flags are recalculated only for affected woodpiece-years", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG1", "WP1_SL1_IMG2", "WP1_SL2_IMG4"))

  x <- suppressMessages(update_QWAdata(x_old, meta))

  # 2004: new overlap IMG2/IMG4, IMG4 has the higher cno
  expect_true(get_ring(x, "WP1_SL1_IMG2", 2004)$duplicate_ring)
  expect_true(get_ring(x, "WP1_SL1_IMG2", 2004)$exclude_dupl)
  expect_false(get_ring(x, "WP1_SL2_IMG4", 2004)$exclude_dupl)
  # 2003: not affected, manual selection of IMG1 is kept
  expect_false(get_ring(x, "WP1_SL1_IMG1", 2003)$exclude_dupl)
  expect_true(get_ring(x, "WP1_SL1_IMG2", 2003)$exclude_dupl)
})

test_that("re-read with reset_manual_flags = TRUE resets manual values", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG1", "WP1_SL1_IMG2", "WP2_SL1_IMG3"))

  expect_message(
    x <- update_QWAdata(x_old, meta, imgs_to_reread = "WP1_SL1_IMG1"),
    "Re-reading 1 image")

  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2001)$cno, 55)
  expect_equal(unique(x$cells$la[x$cells$image_label == "WP1_SL1_IMG1"]), 2)
  expect_false(get_ring(x, "WP1_SL1_IMG1", 2001)$exclude_issues)
  expect_false(get_ring(x, "WP1_SL1_IMG1", 2002)$exclude_issues)
  expect_true(is.na(get_ring(x, "WP1_SL1_IMG1", 2002)$comment))
  # 2003 is affected: manual duplicate selection replaced by auto (IMG2)
  expect_true(get_ring(x, "WP1_SL1_IMG1", 2003)$exclude_dupl)
  expect_false(get_ring(x, "WP1_SL1_IMG2", 2003)$exclude_dupl)
  # other woodpieces untouched
  expect_equal(get_ring(x, "WP2_SL1_IMG3", 2001)$cno, 40)
})

test_that("re-read with reset_manual_flags = FALSE keeps manual edits only", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG1", "WP1_SL1_IMG2", "WP2_SL1_IMG3"))

  x <- suppressMessages(update_QWAdata(x_old, meta,
    imgs_to_reread = "WP1_SL1_IMG1", reset_manual_flags = FALSE))

  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2001)$cno, 55)
  # auto exclusion follows the new flags (no longer incomplete)
  expect_false(get_ring(x, "WP1_SL1_IMG1", 2001)$exclude_issues)
  # manual exclusion and manual columns are kept
  expect_true(get_ring(x, "WP1_SL1_IMG1", 2002)$exclude_issues)
  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2002)$affected_tissue, "decay")
  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2002)$comment, "crack")
  # duplicates are always recalculated for affected woodpiece-years
  expect_true(get_ring(x, "WP1_SL1_IMG1", 2003)$exclude_dupl)
})

test_that("nothing to update returns x unchanged", {
  local_update_mocks()
  meta <- make_meta(unique(rings_old$image_label))

  expect_message(x <- update_QWAdata(x_old, meta), "Nothing to update")
  expect_identical(x, x_old)
})

test_that("without flag columns, images are updated and no flags are added", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG1", "WP1_SL1_IMG2", "WP1_SL2_IMG4"))
  x_noflags <- x_old
  x_noflags$rings <- dplyr::select(x_noflags$rings,
    -c(incomplete_ring, missing_ring, duplicate_ring, exclude_dupl, exclude_issues))

  x <- suppressMessages(update_QWAdata(x_noflags, meta, imgs_to_reread = "WP1_SL1_IMG1"))

  expect_setequal(unique(x$rings$image_label), meta$image_label)
  expect_equal(get_ring(x, "WP1_SL1_IMG1", 2001)$cno, 55)
  # (incomplete_ring and missing_ring come with the mocked raw data)
  expect_false(any(c("duplicate_ring", "exclude_dupl", "exclude_issues") %in% names(x$rings)))
})

test_that("removing an image recalculates the duplicate flags of its overlaps", {
  local_update_mocks()
  meta <- make_meta(c("WP1_SL1_IMG2", "WP2_SL1_IMG3"))

  x <- suppressMessages(update_QWAdata(x_old, meta))

  # 2003: IMG2 (previously the excluded duplicate) no longer overlaps IMG1
  expect_false(get_ring(x, "WP1_SL1_IMG2", 2003)$duplicate_ring)
  expect_false(get_ring(x, "WP1_SL1_IMG2", 2003)$exclude_dupl)
})

# update_QWAimages ----
test_that("update_QWAimages replaces the settings of the re-read images only", {
  testthat::local_mocked_bindings(
    collect_settings_data = function(df = NULL, ...) {
      tibble::tibble(fname_settings = df$fname_settings,
                     meas_created_at = "02.03.2024 12:00:00",
                     spatial_resolution = 2)
    })
  meta <- new_QWAimages(data.frame(
    image_label = c("IMG1", "IMG2"),
    fname_settings = c("IMG1_settings.txt", "IMG2_settings.txt"),
    meas_created_at = as.POSIXct(c("2020-01-01 10:00:00", "2020-01-01 11:00:00"), tz = "UTC"),
    spatial_resolution = 1), "roxas")

  m <- update_QWAimages(meta, "IMG1", settings_date_orders = "dmY HMS",
                        settings_date_tz = "UTC")

  expect_s3_class(m, "QWAimages")
  expect_equal(attr(m, "roxas_version"), "roxas")
  expect_equal(m$spatial_resolution, c(2, 1))
  expect_equal(m$meas_created_at,
               as.POSIXct(c("2024-03-02 12:00:00", "2020-01-01 11:00:00"), tz = "UTC"))
})

# update_QWAmetadata ----
imgs_meta <- tibble::tribble(
  ~site_label, ~species_code, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label, ~only_ew, ~img_comment,
  "S1", "PCAB", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", TRUE,  "blurry",
  "S1", "PCAB", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2", FALSE, NA,
  "S2", "PCAB", "S2_T1", "S2_T1_WP1", "S2_T1_WP1_SL1", "S2_T1_WP1_SL1_IMG1", FALSE, NA,
)
# completed hierarchical tables with user-entered metadata
meta_old <- new_QWAmetadata(
  sites = tibble::tibble(site_label = c("S1", "S2"), n_trees = 1L, site_name = c("Site 1", "Site 2")),
  trees = tibble::tibble(site_label = c("S1", "S2"), species_code = "PCAB",
                         tree_label = c("S1_T1", "S2_T1"), n_woodpieces = 1L, tree_comment = c("old", NA)),
  woodpieces = tibble::tibble(tree_label = c("S1_T1", "S2_T1"), woodpiece_label = c("S1_T1_WP1", "S2_T1_WP1"),
                              n_slides = 1L, pith_year = c(1900, 1950)),
  slides = tibble::tibble(woodpiece_label = c("S1_T1_WP1", "S2_T1_WP1"),
                          slide_label = c("S1_T1_WP1_SL1", "S2_T1_WP1_SL1"), n_images = c(2L, 1L)),
  images = new_QWAimages(imgs_meta, "roxas")
)

expect_hierarchy_ok <- function(m) {
  for (tbl in names(hierarchy_tbls)) {
    h <- hierarchy_tbls[[tbl]]
    expect_true(check_hierarchy(m[[tbl]], tbl, h$id_col, m$images, h$summary_fn))
  }
}

test_that("appended images add rows and update counts, keeping metadata", {
  img_new <- tibble::tribble(
    ~site_label, ~species_code, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label,
    "S1", "PCAB", "S1_T1", "S1_T1_WP2", "S1_T1_WP2_SL1", "S1_T1_WP2_SL1_IMG1",
  )
  images <- new_QWAimages(dplyr::bind_rows(imgs_meta, img_new), "roxas")

  suppressMessages(
    expect_message(m <- update_QWAmetadata(meta_old, images), "woodpieces.*adding 1 row"))

  expect_hierarchy_ok(m)
  expect_equal(m$trees$n_woodpieces[m$trees$tree_label == "S1_T1"], 2L)
  expect_equal(m$trees$tree_comment[m$trees$tree_label == "S1_T1"], "old")
  expect_true(is.na(m$woodpieces$pith_year[m$woodpieces$woodpiece_label == "S1_T1_WP2"]))
  expect_equal(m$sites$site_name, c("Site 1", "Site 2"))
  expect_named(m$woodpieces, names(meta_old$woodpieces))
  expect_s3_class(m$images, "QWAimages")
  expect_equal(attr(m$images, "roxas_version"), "roxas")
})

test_that("missing app columns are carried over from the old images", {
  images <- new_QWAimages(dplyr::select(imgs_meta, -only_ew, -img_comment), "roxas")

  m <- update_QWAmetadata(meta_old, images)

  expect_equal(m$images$only_ew, imgs_meta$only_ew)
  expect_equal(m$images$img_comment, imgs_meta$img_comment)
})

test_that("NA app values are filled from the old images, others kept", {
  images <- imgs_meta |>
    dplyr::mutate(only_ew = NA,
                  img_comment = c("new", NA, NA)) |>
    new_QWAimages("roxas")

  m <- update_QWAmetadata(meta_old, images)

  expect_equal(m$images$only_ew, imgs_meta$only_ew)
  expect_equal(m$images$img_comment, c("new", NA, NA))
})

test_that("removed images remove rows from all hierarchy levels", {
  images <- new_QWAimages(dplyr::filter(imgs_meta, site_label == "S1"), "roxas")

  suppressMessages(
    expect_message(m <- update_QWAmetadata(meta_old, images), "sites.*removing 1 row"))

  expect_hierarchy_ok(m)
  expect_equal(m$sites$site_label, "S1")
  expect_equal(m$slides$slide_label, "S1_T1_WP1_SL1")
})

test_that("NULL hierarchical tables stay NULL", {
  m_in <- meta_old
  m_in["slides"] <- list(NULL)

  m <- update_QWAmetadata(m_in, meta_old$images)

  expect_null(m$slides)
  expect_equal(m$sites, meta_old$sites)
})
