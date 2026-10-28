pattern <- "(?<site>[[:alnum:]]+)_(?<species>[[:alnum:]]+)_(?<tree>[[:alnum:]]+)_(?<slide>[[:alnum:]]+)_(?<image>[[:alnum:]]+)"

test_that("org_img_name excludes path and image extension", {
  filenames <- c(
    "path/to/SITEA_PISY_01_1_1.jpg",
    "path/to/SITEA_PISY_01_1_2.JPEG",
    "path/to/SITEA_PISY_01_1_3.scan.png",
    "SITEA_PISY_01_1_4"
  )
  df <- suppressMessages(get_structure_from_filenames(filenames, pattern))

  expect_equal(
    df$org_img_name,
    c("SITEA_PISY_01_1_1", "SITEA_PISY_01_1_2",
      "SITEA_PISY_01_1_3", "SITEA_PISY_01_1_4")
  )
})

test_that("strip_img_ext only removes trailing image extensions", {
  expect_equal(
    strip_img_ext(c("a.scan.jpg", "a.tif", "a.scan", "a.txt", "a.jpg.bak")),
    c("a", "a", "a.scan", "a.txt", "a.jpg.bak")
  )
})
