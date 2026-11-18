# settings reader replaced by a minimal one, as there are no raw ROXAS fixtures;
# files in `unreadable` warn and return the reader's fallback
local_settings_mock <- function(unreadable = character(0), env = parent.frame()) {
  testthat::local_mocked_bindings(
    read_roxas_settings = function(file_settings, roxas_version) {
      if (is.na(file_settings) || file_settings %in% unreadable) {
        if (!is.na(file_settings)) cli::cli_warn("Could not read file {file_settings}")
        return(tibble::tibble(fname_settings = NA_character_, software = roxas_version))
      }
      tibble::tibble(fname_settings = file_settings, software = roxas_version,
                     meas_geometry = "1", spatial_resolution = "2")
    },
    .env = env
  )
}

# existing (empty) files in the session temp dir, as non-existing ones count
# as missing
local_files <- function(n, ext) {
  files <- tempfile(fileext = rep(ext, n))
  fs::file_create(files)
  files
}
rxs_settings <- function(n) local_files(n, "_ROXAS_Settings.txt")
rai_settings <- function(n) local_files(n, ".metadata.json")

# collect all warnings raised by `expr` (muffled)
capture_warnings_msgs <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  msgs
}

test_that("classic ROXAS: images without a file keep empty image metadata", {
  local_settings_mock()
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images, img_filetype = "JPEG",
                     img_size = seq_along(files_images) * 100,
                     img_created_at = "2020:01:01 10:00:00")
    })
  imgs <- local_files(2, ".jpg")
  files_settings <- rxs_settings(3)

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = files_settings,
      files_images = c(imgs[1], NA, imgs[2]))),
    "image file name missing")

  expect_equal(res$fname_settings, files_settings)
  expect_equal(res$img_size, c(100L, NA, 200L))
  expect_equal(res$img_filetype, c("JPEG", NA, "JPEG"))
  expect_equal(is.na(res$img_created_at), c(FALSE, TRUE, FALSE))
})

test_that("classic ROXAS: no image file at all", {
  local_settings_mock()

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = rxs_settings(2),
      files_images = c(NA, NA))), # logical NA
    "image file name missing")

  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$img_size)))
})

test_that("classic ROXAS: non-existing image files are handled like missing ones", {
  local_settings_mock()
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images, img_filetype = "JPEG")
    })

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = rxs_settings(2),
      files_images = c(local_files(1, ".jpg"), "does/not/exist.jpg"))),
    "image file not found")

  expect_equal(res$img_filetype, c("JPEG", NA))
})

test_that("classic ROXAS: images without a settings file keep empty settings", {
  local_settings_mock()
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images, img_filetype = "JPEG",
                     img_size = 100, img_created_at = "2020:01:01 10:00:00")
    })
  imgs <- local_files(2, ".jpg")
  files_settings <- c(rxs_settings(1), NA)

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = files_settings, files_images = imgs)),
    "settings file name missing")

  expect_equal(res$fname_settings, files_settings)
  expect_equal(res$fname_image, imgs)
  expect_equal(res$spatial_resolution, c(2, NA))
  expect_equal(res$software, c("roxas", "roxas"))
  expect_equal(res$img_filetype, c("JPEG", "JPEG"))
})

test_that("non-existing settings files are handled like missing ones", {
  local_settings_mock()
  files_settings <- c(rxs_settings(1), "does/not/exist_ROXAS_Settings.txt")

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = files_settings, files_images = c(NA, NA))),
    "settings file not found")

  expect_equal(res$fname_settings, files_settings)
  expect_equal(res$spatial_resolution, c(2, NA))
})

test_that("unreadable settings files keep empty settings and their file name", {
  files_settings <- rxs_settings(2)
  local_settings_mock(unreadable = files_settings[2])
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images)
    })

  # only the reader's warning: unreadable files are not listed again
  warns <- capture_warnings_msgs(
    res <- suppressMessages(collect_settings_data(
      files_settings = files_settings, files_images = local_files(2, ".jpg"))))

  expect_length(warns, 1)
  expect_match(warns, "Could not read file")
  expect_equal(res$fname_settings, files_settings)
  expect_equal(res$spatial_resolution, c(2, NA))
})

test_that("missing files are grouped in one warning, listed by label", {
  local_settings_mock()
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images)
    })
  df <- tibble::tibble(
    image_label = paste0("IMG", 1:4),
    fname_settings = c(rxs_settings(2), NA, NA),
    fname_image = c(NA, local_files(1, ".jpg"), NA, "does/not/exist.jpg")
  )

  warns <- capture_warnings_msgs(suppressMessages(collect_settings_data(df)))

  expect_length(warns, 1)
  expect_match(warns, "3 images with missing files")
  expect_match(warns, "image file name missing \\(n = 1\\)\\n\\s*IMG1")
  expect_match(warns, "image file name missing, settings file name missing \\(n = 1\\)\\n\\s*IMG3")
  expect_match(warns, "image file not found, settings file name missing \\(n = 1\\)\\n\\s*IMG4")
})

test_that("missing files are listed by file name without labels", {
  local_settings_mock()
  files_settings <- rxs_settings(2)

  expect_warning(
    suppressMessages(collect_settings_data(
      files_settings = files_settings, files_images = c(NA, NA))),
    basename(files_settings[1]))
})

test_that("aborts if no settings file can be read", {
  files_settings <- rxs_settings(2)
  local_settings_mock(unreadable = files_settings)

  expect_error(
    suppressWarnings(suppressMessages(collect_settings_data(
      files_settings = files_settings, files_images = c(NA, NA)))),
    "None of the ROXAS settings files could be found or read")
})

test_that("ROXAS AI: image metadata from the EXIF for images without a settings file", {
  local_settings_mock()
  testthat::local_mocked_bindings(
    collect_image_info = function(files_images, ...) {
      tibble::tibble(fname_image = files_images, img_filetype = "JPEG",
                     img_size = 1, img_created_at = "2020:01:01 10:00:00")
    })
  imgs <- local_files(2, ".scan.jpg")

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = c(rai_settings(1), NA), files_images = imgs)),
    "settings file name missing")

  expect_equal(res$img_filetype, c(NA, "JPEG"))
  expect_equal(is.na(res$img_created_at), c(TRUE, FALSE))
  expect_false(anyNA(res$img_size))
  expect_equal(res$software, c("roxas_ai", "roxas_ai"))
})

test_that("ROXAS AI: images without a file have no image size", {
  local_settings_mock()
  img <- tempfile(fileext = ".scan.jpg")
  writeLines("abc", img)

  expect_warning(
    res <- suppressMessages(collect_settings_data(
      files_settings = rai_settings(2),
      files_images = c(img, NA))),
    "image file name missing")

  expect_equal(is.na(res$img_size), c(FALSE, TRUE))
})

test_that("collect_image_info: no files gives the typed empty result", {
  res <- collect_image_info(character(0))

  expect_equal(nrow(res), 0)
  expect_named(res, c("fname_image", "img_filetype", "img_software",
                      "img_size", "img_width", "img_height", "img_created_at"),
               ignore.order = TRUE)
  expect_type(res$img_width, "double")
})

test_that("collect_image_info: unreadable files warn and keep input paths and order", {
  skip_if(Sys.which("perl") == "", "exiftool requires perl")
  empty <- local_files(1, ".jpg") # exiftool: "File is empty"
  text <- tempfile(fileext = ".jpg")
  writeLines("not an image", text) # readable, detected as text

  warns <- capture_warnings_msgs(
    res <- suppressMessages(collect_image_info(c(text, empty))))

  expect_length(warns, 1)
  expect_match(warns, "Could not read file.*File is empty")
  expect_equal(res$fname_image, c(text, empty))
  expect_equal(res$img_filetype, c("TXT", NA))
  expect_true(all(is.na(res$img_width))) # absent tag, typed column
  expect_type(res$img_width, "double")
})
