meta_example <- suppressMessages(suppressWarnings(read_QWAmetadata(
  system.file("extdata", "TRIA_example_QWAmetadata.json", package = "rxs2tria"))))

# write x to a temp JSON file, return the file content as a list of rows
write_read_raw <- function(x, ...) {
  file <- tempfile(fileext = ".json")
  suppressMessages(write_QWAmetadata(x, file, ...))
  jsonlite::read_json(file, simplifyVector = FALSE)
}

test_that("write_QWAmetadata writes empty values as null, never as empty strings", {
  x <- meta_example
  x$sites$site_desc[1] <- ""
  file <- tempfile(fileext = ".json")
  suppressMessages(write_QWAmetadata(x, file))

  expect_false(any(grepl('""', readLines(file))))
  raw <- jsonlite::read_json(file, simplifyVector = FALSE)
  expect_null(raw$sites[[1]]$site_desc)
  expect_true("site_desc" %in% names(raw$sites[[1]]))
})

test_that("write_QWAmetadata drops all-empty optional meta_input fields only", {
  x <- meta_example
  x$dataset$description <- NA_character_ # required
  raw <- write_read_raw(x)

  expect_false(any(c("aff_department", "aff_street", "aff_plz") %in% names(raw$authors[[1]])))
  expect_false(any(c("embargoed_until", "acknowledgements") %in% names(raw$dataset[[1]])))
  expect_true("description" %in% names(raw$dataset[[1]]))
  expect_true("n_trees" %in% names(raw$sites[[1]])) # derived

  raw <- write_read_raw(x, drop_empty = FALSE)
  expect_true("aff_department" %in% names(raw$authors[[1]]))
  expect_true("acknowledgements" %in% names(raw$dataset[[1]]))
})

test_that("write_QWAmetadata keeps empty conditionally required fields where they apply", {
  # public: license kept if empty, embargo date dropped
  x <- meta_example
  x$dataset$ds_license <- NA_character_
  x$dataset$embargoed_until <- NA_character_
  ds <- write_read_raw(x)$dataset[[1]]
  expect_true("ds_license" %in% names(ds))
  expect_false("embargoed_until" %in% names(ds))

  # restricted: embargo date kept if empty, license dropped
  x$dataset$ds_access <- "restricted"
  ds <- write_read_raw(x)$dataset[[1]]
  expect_true("embargoed_until" %in% names(ds))
  expect_false("ds_license" %in% names(ds))
})

test_that("written QWAmetadata reads back with the same values", {
  file <- tempfile(fileext = ".json")
  suppressMessages(write_QWAmetadata(meta_example, file))
  y <- suppressMessages(suppressWarnings(read_QWAmetadata(file)))

  for (comp in c("dataset", "authors", "funding", "sites", "trees", "woodpieces", "slides")) {
    cols <- names(y[[comp]])
    expect_equal(y[[comp]], meta_example[[comp]][cols], label = comp)
  }
  expect_equal(nrow(y$images), nrow(meta_example$images))
})

test_that("write_QWAdata writes NA as empty fields and drops all-empty qc_input columns", {
  cells <- tibble::tibble(image_label = "IMG1", year = 2000, xpix = c(1, 2), ypix = c(1, 2),
                          la = c(10, NA), cwttan = NA_real_)
  rings <- tibble::tibble(image_label = "IMG1", year = c(2000, 2001), mrw = NA_real_,
                          x_dating = c(TRUE, NA), comment = c(NA, ""))
  dir <- tempfile()
  dir.create(dir)
  files <- suppressMessages(write_QWAdata(new_QWAdata(cells, rings), dir, compress = FALSE))

  rings_txt <- readLines(files$file_rings)
  expect_equal(rings_txt[1], "image_label,year,mrw,x_dating") # mrw kept, comment dropped
  expect_false(any(grepl("NA", rings_txt)))
  cells_txt <- readLines(files$file_cells)
  expect_equal(cells_txt[1], "image_label,year,xpix,ypix,la,cwttan")
  expect_false(any(grepl("NA", cells_txt)))

  files <- suppressMessages(write_QWAdata(new_QWAdata(cells, rings), dir, compress = FALSE,
                                          overwrite = TRUE, drop_empty = FALSE))
  expect_equal(readLines(files$file_rings)[1], "image_label,year,mrw,x_dating,comment")
})
