imgs <- tibble::tribble(
  ~site_label, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label, ~org_img_name, ~software,
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", "S1_T1_WP1_SL1_IMG1", "roxas",
)

test_that("recompile_supplementary flags 'other' rows when description column is absent", {
  dir <- tempfile()
  fs::dir_create(dir)
  on.exit(fs::dir_delete(dir), add = TRUE)
  fs::file_create(fs::path(dir, "notes.xyz"))

  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- tibble::tibble(
    resource_name = "notes.xyz", resource_type = "other",
    linked_level = NA_character_, linked_label = NA_character_
  )

  res <- suppressMessages(suppressWarnings(
    recompile_supplementary(suppl_res, dir, rxs_images)
  ))

  expect_true("description" %in% names(res))
  expect_equal(res$status, "review")
})
