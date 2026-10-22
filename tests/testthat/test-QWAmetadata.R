read_quiet <- function(file) suppressMessages(read_QWAmetadata(file))

sub_file <- system.file("extdata", "TRIA_example_QWAmetadata.json", package = "rxs2tria")
tria_file <- write_raw_json(tria_example_raw())

test_that("the stage is inferred from the components", {
  expect_true(is_submission(suppressWarnings(read_quiet(sub_file))))
  expect_false(is_submission(read_quiet(tria_file)))

  raw <- tria_example_raw()
  expect_false(is_submission(suppressMessages(as_QWAmetadata(raw[names(raw) != "datafiles"]))))
  expect_false(is_submission(suppressMessages(as_QWAmetadata(raw[c("datafiles", "images")]))))
})

test_that("validate_QWAmetadata aborts on invalid objects", {
  x <- read_quiet(tria_file)
  expect_identical(validate_QWAmetadata(x), x)

  bad <- x
  bad$sites <- "not a table"
  expect_error(validate_QWAmetadata(bad), "sites")
  bad <- x
  attr(bad, "submission") <- NA
  expect_error(validate_QWAmetadata(bad), "submission")
  bad <- x
  bad$images$image_label[1] <- NA
  expect_error(validate_QWAmetadata(bad), "missing values")
})

test_that("TRIA dataset files read and check without warnings", {
  expect_no_warning(x <- read_quiet(tria_file))
  expect_equal(x$dataset$ds_label, "TRIA_test")
  expect_equal(x$dataset$dataset_version, 1L)
  expect_true(all(c("size_bytes", "checksum", "s3_key") %in% names(x$datafiles)))
  expect_null(x$related)
  expect_no_warning(suppressMessages(check_QWAmetadata(x)))
})

test_that("related is dropped from TRIA dataset files", {
  raw <- tria_example_raw()
  raw$related <- jsonlite::read_json(sub_file, simplifyVector = TRUE)$related
  expect_warning(x <- suppressMessages(as_QWAmetadata(raw)), "related")
  expect_null(x$related)
})

test_that("checks use the schemas of the stage", {
  x <- read_quiet(tria_file)
  x$datafiles$checksum[1] <- "not_md5"
  expect_warning(suppressMessages(check_QWAmetadata(x)), "failed validation")

  imgs <- new_QWAimages(dplyr::mutate(tibble::as_tibble(x$images), fname_image = "img.jpg"), "roxas")
  expect_no_warning(suppressMessages(check_QWAimages(imgs)))
  expect_warning(suppressMessages(check_QWAimages(imgs, submission = FALSE)), "failed validation")
})

test_that("check_QWAimages returns the pass state and aborts with strict", {
  imgs <- read_quiet(tria_file)$images
  expect_true(suppressMessages(check_QWAimages(imgs, submission = FALSE)))

  future <- imgs
  future$outmost_year[1] <- 3000L
  expect_warning(res <- suppressMessages(check_QWAimages(future, submission = FALSE)), "in the future")
  expect_false(res)
  expect_error(suppressMessages(check_QWAimages(future, submission = FALSE, strict = TRUE)), "in the future")

  no_res <- imgs
  no_res$spatial_resolution[1] <- NA
  expect_warning(res <- suppressMessages(check_QWAimages(no_res, submission = FALSE)), "spatial_resolution")
  expect_false(res)
})

test_that("check_QWAimages informs on empty values, except fname_* and EXIF columns", {
  imgs <- read_quiet(tria_file)$images
  imgs$sw_version[1] <- NA
  imgs$img_software <- NA_character_
  msgs <- capture_messages(check_QWAimages(imgs, submission = FALSE))
  expect_match(msgs, "sw_version` \\(1 row\\)", all = FALSE)
  expect_no_match(paste(msgs, collapse = ""), "img_software|fname_")
})

test_that("check_QWAmetadata skips absent hierarchy tables", {
  x <- read_quiet(tria_file)
  x$sites <- NULL
  msgs <- capture_messages(check_QWAmetadata(x))
  expect_match(msgs, "sites.*not provided", all = FALSE)
})

test_that("QWAmetadata round-trips at both stages", {
  comps <- c("dataset", "authors", "funding", "related", "datafiles",
             "sites", "trees", "woodpieces", "slides")
  for (f in c(sub_file, tria_file)) {
    x <- suppressWarnings(read_quiet(f))
    out <- tempfile(fileext = ".json")
    suppressMessages(write_QWAmetadata(x, out))
    absent <- setdiff(c("related", "datafiles"), names(jsonlite::read_json(out)))
    expect_equal(absent, if (is_submission(x)) "datafiles" else "related")

    y <- suppressWarnings(read_quiet(out))
    expect_equal(is_submission(y), is_submission(x))
    for (comp in comps) {
      if (is.null(x[[comp]])) next
      expect_equal(y[[comp]], x[[comp]][names(y[[comp]])], label = comp)
    }
    expect_equal(names(y$images), names(x$images))
  }
})

test_that("TRIA dataset files are not edited", {
  x <- read_quiet(tria_file)
  expect_error(complete_QWAmetadata(x), "not edited")
  expect_error(update_QWAmetadata(x, x$images), "not edited")
  expect_error(combine_QWAmetadata(x, x), "not edited")
})

test_that("print shows the stage", {
  expect_snapshot(print(suppressWarnings(read_quiet(sub_file))))
  expect_snapshot(print(read_quiet(tria_file)))
})
