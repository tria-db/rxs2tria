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

test_that("check_supplementary warns on non-junk rows missing from path, recompile drops them", {
  dir <- make_suppl_dir(c("S1_T1_WP1_SL1_IMG1_Output_Cells.txt", "notes.xyz",
                          "S1_T1_WP1_SL1_IMG1_Vessels_bu.scl"))
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(suppressWarnings(compile_supplementary(dir, rxs_images)))
  fs::file_delete(fs::path(dir, c("notes.xyz", "S1_T1_WP1_SL1_IMG1_Vessels_bu.scl")))

  w <- capture_warnings(ok <- suppressMessages(check_supplementary(suppl_res, dir, rxs_images)))
  expect_match(w, "not found in", all = FALSE)
  expect_no_match(paste(w, collapse = ""), "Vessels_bu")
  expect_false(ok)

  expect_no_warning(res <- suppressMessages(recompile_supplementary(suppl_res, dir, rxs_images)))
  expect_equal(res$resource_name, "S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
})

test_that("check_supplementary warns on undeclared files, except junk", {
  dir <- make_suppl_dir("S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  on.exit(fs::dir_delete(dir), add = TRUE)
  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))

  fs::file_create(fs::path(dir, "S1_T1_WP1_SL1_IMG1_Vessels_bu.scl"))
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

test_that("zip of the folder (with system files) matches a manifest compiled from the folder", {
  skip_if_not_installed("zip")
  parent <- tempfile()
  on.exit(fs::dir_delete(parent), add = TRUE)
  dir <- fs::path(parent, "supp")
  files <- c("S1_T1_WP1_SL1_IMG1_Output_Cells.txt", "sub/S1_T1_WP1_SL1_IMG1_Vessels_bu.scl")
  fs::dir_create(fs::path(dir, "sub"))
  fs::file_create(fs::path(dir, c(files, ".DS_Store")))
  # Finder-style sidecars, next to the zipped folder
  fs::dir_create(fs::path(parent, "__MACOSX", "supp"))
  fs::file_create(fs::path(parent, "__MACOSX", "supp", "._S1_T1_WP1_SL1_IMG1_Output_Cells.txt"))
  zipfile <- fs::path(parent, "supp.zip")
  zip::zip(zipfile, files = c("supp", "__MACOSX"), root = parent)

  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))
  expect_setequal(suppl_res$resource_name, files)
  from_zip <- suppressMessages(compile_supplementary(zipfile, rxs_images))
  expect_setequal(from_zip$resource_name, files)

  expect_no_warning(
    ok <- suppressMessages(check_supplementary(suppl_res, zipfile, rxs_images))
  )
  expect_true(ok)
})

test_that("a single subfolder holding all files is dropped from folder and zip scans", {
  skip_if_not_installed("zip")
  dir <- tempfile()
  on.exit(fs::dir_delete(dir), add = TRUE)
  fs::dir_create(fs::path(dir, "images"))
  fs::file_create(fs::path(dir, "images", "S1_T1_WP1_SL1_IMG1_Output_Cells.txt"))
  zipfile <- tempfile(fileext = ".zip")
  on.exit(fs::file_delete(zipfile), add = TRUE)
  zip::zip(zipfile, files = "images", root = dir) # zip of the folder's contents

  rxs_images <- suppressMessages(QWAimages(imgs))
  suppl_res <- suppressMessages(compile_supplementary(dir, rxs_images))
  expect_equal(suppl_res$resource_name, "S1_T1_WP1_SL1_IMG1_Output_Cells.txt")
  expect_true(suppressMessages(check_supplementary(suppl_res, zipfile, rxs_images)))
})
