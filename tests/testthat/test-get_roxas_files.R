prefix <- "S1_T1_WP1_SL1_IMG1"
classic_outputs <- paste0(prefix, c("_ROXAS_Settings.txt", "_Output_Cells.txt", "_Output_Rings.txt"))

# create a temporary directory containing the given (empty) files
make_files_dir <- function(files) {
  dir <- tempfile()
  fs::dir_create(dir)
  fs::file_create(fs::path(dir, files))
  dir
}

test_that("a complete set of classic ROXAS files is found", {
  dir <- make_files_dir(c(paste0(prefix, ".jpg"), classic_outputs))
  on.exit(fs::dir_delete(dir), add = TRUE)

  expect_message(res <- get_roxas_files(dir, "roxas"), "complete set")
  expect_equal(nrow(res), 1)
  expect_equal(basename(res$prefix), prefix)
})

test_that("the highest-priority image format is kept", {
  dir <- make_files_dir(c(paste0(prefix, c(".jpg", ".tif")), classic_outputs))
  on.exit(fs::dir_delete(dir), add = TRUE)

  msgs <- capture_messages(res <- get_roxas_files(dir, "roxas"))
  expect_match(msgs, "lower-priority", all = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(basename(res$fname_image), paste0(prefix, ".tif"))
})

test_that("image extensions are matched case-insensitively", {
  dir <- make_files_dir(c(paste0(prefix, ".JPG"), classic_outputs))
  on.exit(fs::dir_delete(dir), add = TRUE)

  res <- suppressMessages(get_roxas_files(dir, "roxas"))
  expect_equal(basename(res$prefix), prefix)
  expect_equal(basename(res$fname_image), paste0(prefix, ".JPG"))
})

test_that("png images are not accepted for classic ROXAS", {
  dir <- make_files_dir(c(paste0(prefix, ".png"), classic_outputs))
  on.exit(fs::dir_delete(dir), add = TRUE)

  expect_warning(res <- suppressMessages(get_roxas_files(dir, "roxas")), "Missing image")
  expect_true(is.na(res$fname_image))
})

test_that("incomplete sets are returned and grouped by their missing files", {
  other <- "S1_T1_WP1_SL1_IMG2"
  dir <- make_files_dir(c(
    classic_outputs,                                         # no image
    paste0(other, c(".jpg", "_ROXAS_Settings.txt")),         # no cells, rings
    paste0("S1_T1_WP1_SL1_IMG3", c(".jpg", "_ROXAS_Settings.txt",
                                   "_Output_Cells.txt", "_Output_Rings.txt"))
  ))
  on.exit(fs::dir_delete(dir), add = TRUE)

  msgs <- capture_messages(
    expect_warning(res <- get_roxas_files(dir, "roxas"),
                   "2 incomplete sets.*Missing cells, rings.*Missing image"))
  expect_equal(nrow(res), 3)
  expect_match(msgs, "1 complete set of ROXAS files and 2 with issues", all = FALSE)
})

test_that("multiple analysis results per image are kept with a warning", {
  dir <- make_files_dir(paste0(prefix, c(
    ".scan.jpg", ".metadata.json",
    ".cells_table.csv", ".cells_table.txt",
    ".rings_table.csv", ".rings_table.txt"
  )))
  on.exit(fs::dir_delete(dir), add = TRUE)

  msgs <- capture_messages(
    expect_warning(res <- get_roxas_files(dir, "roxas_ai"), "multiple .* analysis results"))
  expect_match(msgs, "0 complete sets .* and 1 with issues", all = FALSE)
  expect_equal(nrow(res), 4)
  expect_equal(
    sort(basename(unique(res$fname_cells))),
    paste0(prefix, c(".cells_table.csv", ".cells_table.txt"))
  )
})
