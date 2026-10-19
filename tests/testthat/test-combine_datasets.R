# combine_QWAimages ----
imgs_example <- suppressMessages(suppressWarnings(read_QWAmetadata(
  system.file("extdata", "TRIA_example_QWAmetadata.json", package = "rxs2tria"))))$images
wps <- unique(imgs_example$woodpiece_label)
imgs_x <- dplyr::filter(imgs_example, woodpiece_label == wps[1])
imgs_y <- dplyr::filter(imgs_example, woodpiece_label != wps[1])

test_that("combine_QWAimages combines images from both objects", {
  m <- suppressMessages(suppressWarnings(combine_QWAimages(imgs_x, imgs_y)))

  expect_s3_class(m, "QWAimages")
  expect_equal(attr(m, "roxas_version"), attr(imgs_example, "roxas_version"))
  expect_setequal(m$image_label, imgs_example$image_label)
})

# combine_QWAdata ----
# x: WP1 IMG0/IMG1 overlap in 2001, user manually selected IMG0 (lower cno)
rings_x <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~cno, ~incomplete_ring, ~missing_ring, ~duplicate_ring, ~exclude_dupl, ~exclude_issues, ~comment,
  "WP1", "WP1_SL1", "WP1_SL1_IMG0", 2001, 40, FALSE, FALSE, TRUE,  FALSE, FALSE, "manual",
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 50, FALSE, FALSE, TRUE,  TRUE,  FALSE, NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002, 60, FALSE, FALSE, FALSE, FALSE, FALSE, NA,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 70, FALSE, FALSE, FALSE, FALSE, FALSE, NA,
)
# y: WP1 IMG2 overlaps x in 2003 with a higher cno; WP2 only in y
rings_y <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~cno, ~incomplete_ring, ~missing_ring, ~duplicate_ring, ~exclude_dupl, ~exclude_issues,
  "WP1", "WP1_SL2", "WP1_SL2_IMG2", 2003, 80, FALSE, FALSE, FALSE, FALSE, FALSE,
  "WP1", "WP1_SL2", "WP1_SL2_IMG2", 2004, 90, FALSE, FALSE, FALSE, FALSE, FALSE,
  "WP2", "WP2_SL1", "WP2_SL1_IMG3", 2001, 30, FALSE, FALSE, FALSE, FALSE, TRUE,
)
make_data <- function(rings) {
  new_QWAdata(
    cells = rings |> dplyr::distinct(image_label, year) |> dplyr::mutate(la = 1),
    rings = rings)
}
data_x <- make_data(rings_x)
data_y <- make_data(rings_y)

get_ring <- function(x, img, yr) {
  x$rings |> dplyr::filter(image_label == img, year == yr)
}

test_that("combine_QWAdata combines rings and cells", {
  expect_message(m <- combine_QWAdata(data_x, data_y), "only in one object")

  expect_s3_class(m, "QWAdata")
  expect_equal(nrow(m$rings), nrow(rings_x) + nrow(rings_y))
  expect_equal(nrow(m$cells), nrow(rings_x) + nrow(rings_y))
  expect_true(is.na(get_ring(m, "WP1_SL2_IMG2", 2004)$comment))
  expect_true(get_ring(m, "WP2_SL1_IMG3", 2001)$exclude_issues)
})

test_that("combine_QWAdata recalculates duplicates only for shared woodpiece-years", {
  m <- suppressMessages(combine_QWAdata(data_x, data_y))

  # 2003: new overlap between x and y, IMG2 has the higher cno
  expect_true(get_ring(m, "WP1_SL1_IMG1", 2003)$duplicate_ring)
  expect_true(get_ring(m, "WP1_SL1_IMG1", 2003)$exclude_dupl)
  expect_false(get_ring(m, "WP1_SL2_IMG2", 2003)$exclude_dupl)
  # 2001: only in x, manual selection of IMG0 is kept
  expect_false(get_ring(m, "WP1_SL1_IMG0", 2001)$exclude_dupl)
  expect_true(get_ring(m, "WP1_SL1_IMG1", 2001)$exclude_dupl)
})

# combine_QWAmetadata ----
meta_example <- suppressMessages(suppressWarnings(read_QWAmetadata(
  system.file("extdata", "TRIA_example_QWAmetadata.json", package = "rxs2tria"))))

# subset of the example metadata for the given trees (site POG is in both)
subset_meta <- function(trees) {
  imgs <- dplyr::filter(meta_example$images, tree_label %in% trees)
  m <- meta_example
  for (tbl in names(hierarchy_tbls)) {
    id_col <- hierarchy_tbls[[tbl]]$id_col
    m[[tbl]] <- dplyr::filter(m[[tbl]], .data[[id_col]] %in% imgs[[id_col]])
  }
  suppressMessages(update_QWAmetadata(m, imgs))
}
meta_x <- subset_meta(c("POG_PISY_02", "STO_PISY_20"))
meta_y <- subset_meta("POG_PISY_08")
# y: one additional author (as contact person) and related object
meta_y$authors <- dplyr::bind_rows(meta_y$authors,
  tibble::tibble(author_nr = 0, last_name = "New", first_name = "Author",
                 email = "new@wsl.ch", contact_person = TRUE))
meta_y$related <- dplyr::bind_rows(meta_y$related,
  tibble::tibble(citation = "Another citation"))

test_that("combine_QWAmetadata combines images and hierarchy with updated counts", {
  suppressMessages(
    expect_message(m <- suppressWarnings(combine_QWAmetadata(meta_x, meta_y)),
                   "dataset.*kept from"))

  expect_setequal(m$images$image_label, meta_example$images$image_label)
  for (tbl in names(hierarchy_tbls)) {
    h <- hierarchy_tbls[[tbl]]
    expect_true(check_hierarchy(m[[tbl]], tbl, h$id_col, m$images, h$summary_fn))
  }
  expect_equal(m$sites$n_trees[m$sites$site_label == "POG"], 2)
  expect_equal(m$dataset, meta_x$dataset)
  expect_null(m$datafiles)
})

test_that("combine_QWAmetadata deduplicates authors, funding and related", {
  m <- suppressMessages(suppressWarnings(combine_QWAmetadata(meta_x, meta_y)))

  expect_equal(m$authors$email, c(meta_x$authors$email, "new@wsl.ch"))
  expect_equal(m$authors$author_nr, seq_len(nrow(meta_x$authors) + 1))
  expect_equal(m$authors$email[m$authors$contact_person], "abcdf@wsl.ch")
  expect_equal(nrow(m$funding), nrow(meta_x$funding))
  expect_equal(m$related$citation, c(meta_x$related$citation, "Another citation"))
})

test_that("combine_QWAmetadata warns on shared IDs with different values, x wins", {
  meta_y2 <- meta_y
  meta_y2$sites$site_name <- "changed"

  expect_warning(m <- suppressMessages(combine_QWAmetadata(meta_x, meta_y2)),
                 "sites.*shared ID")
  expect_equal(m$sites$site_name, meta_x$sites$site_name)
})
