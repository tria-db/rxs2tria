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

make_suppl_dir <- function(files) {
  dir <- tempfile()
  fs::dir_create(dir)
  fs::file_create(fs::path(dir, files))
  dir
}

test_that("check_supplementary passes a manifest with extra columns", {
  dir <- make_suppl_dir("S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))
  expect_equal(suppl_res$status, "ok")

  suppl_res$my_note <- "kept by hand"
  expect_no_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images))
  )
  expect_true(ok)
})

test_that("check_supplementary warns or aborts on schema issues", {
  dir <- make_suppl_dir("S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))
  suppl_res$fname_resource <- 1

  expect_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images)),
    "failed validation"
  )
  expect_false(ok)
  expect_error(
    suppressMessages(check_supplementary(suppl_res, dir, rxs_images, strict = TRUE)),
    "failed validation"
  )
})

test_that("compile_supplementary leaves ambiguous linked_label NA", {
  dir <- make_suppl_dir("T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  imgs_dup <- tibble::tribble(
    ~site_label, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label, ~org_img_name, ~software,
    "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", "T1_WP1_SL1_IMG1", "roxas",
    "S2", "S2_T1", "S2_T1_WP1", "S2_T1_WP1_SL1", "S2_T1_WP1_SL1_IMG1", "T1_WP1_SL1_IMG1", "roxas",
  )
  rxs_images <- suppressMessages(suppressWarnings(QWAimages(imgs_dup)))

  suppl_res <- suppressMessages(suppressWarnings(compile_supplementary(dir, rxs_images)))
  expect_true(is.na(suppl_res$linked_label))
  expect_equal(suppl_res$status, "review")
})

test_that("check_supplementary warns on 'ok' files missing from path, recompile aborts", {
  dir <- make_suppl_dir("S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))
  fs::file_delete(fs::path(dir, "S1_T1_WP1_SL1_IMG1_Output_Cells.txt"))
  fs::file_create(fs::path(dir, "Thumbs.db")) # keeps path non-empty

  expect_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images)),
    "not found in"
  )
  expect_false(ok)
  expect_error(
    suppressMessages(check_supplementary(suppl_res, dir, rxs_images, strict = TRUE)),
    "not found in"
  )
  expect_error(
    suppressMessages(recompile_supplementary(suppl_res, dir, rxs_images)),
    "not found in"
  )
})

test_that("check_supplementary warns on undeclared files, except junk", {
  dir <- make_suppl_dir("S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))

  fs::file_create(fs::path(dir, "Thumbs.db"))
  expect_no_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images))
  )
  expect_true(ok)

  fs::file_create(fs::path(dir, "notes.xyz"))
  expect_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images)),
    "not declared"
  )
  expect_false(ok)
  expect_error(
    suppressMessages(check_supplementary(suppl_res, dir, rxs_images, strict = TRUE)),
    "not declared"
  )
  # recompile ignores them (information only)
  expect_no_warning(suppressMessages(recompile_supplementary(suppl_res, dir, rxs_images)))
})

test_that("check_supplementary warns or aborts on resources that need review", {
  dir <- make_suppl_dir(c("S1_T1_WP1_SL1_IMG1_Output_Cells.txt", "notes.xyz"))
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(suppressWarnings(compile_supplementary(dir, rxs_images)))
  expect_equal(sort(suppl_res$status), c("ok", "review"))

  expect_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images)),
    "need review"
  )
  expect_false(ok)
  expect_error(
    suppressMessages(check_supplementary(suppl_res, dir, rxs_images, strict = TRUE)),
    "need review"
  )
})
