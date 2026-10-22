structure_df <- tibble::tribble(
  ~site_label, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label,
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1",
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2",
)

test_that("report_issue warns by default, aborts with strict, in caller's env", {
  f <- function(strict) {
    n_img <- 3
    report_issue(c("!" = "{n_img} images affected"), strict)
  }
  expect_warning(res <- f(FALSE), "3 images affected")
  expect_false(res)
  expect_error(f(TRUE), "3 images affected")
})

test_that("check_structure warns on prefix mismatch, aborts with strict", {
  expect_true(check_structure(structure_df))

  mismatch <- structure_df
  mismatch$image_label[2] <- "X_IMG2"
  expect_warning(res <- check_structure(mismatch), "X_IMG2")
  expect_false(res)
  expect_error(check_structure(mismatch, strict = TRUE), "X_IMG2")
})

test_that("align_to_schema drops extra columns unless keep_extra", {
  schema_path <- system.file(schema_rel_path("rings"), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- get_tbl_props(resolve_schema(schema_obj, schema_path))
  rings <- data.frame(
    woodpiece_label = "WP1", slide_label = "WP1_SL1", image_label = "WP1_SL1_IMG1",
    year = c("2000", "2001"), my_eww = c(1L, 2L)
  )

  expect_warning(res <- align_to_schema(rings, tbl_props, "rings"), "my_eww")
  expect_false("my_eww" %in% names(res))

  expect_message(
    res <- align_to_schema(rings, tbl_props, "rings", keep_extra = TRUE),
    "extra columns kept"
  )
  expect_equal(names(res), c("woodpiece_label", "slide_label", "image_label", "year", "my_eww"))
  expect_identical(res$my_eww, c(1L, 2L))
  expect_identical(res$year, c(2000L, 2001L))
})

test_that("QWAimages aborts on duplicate image_labels", {
  imgs <- dplyr::mutate(structure_df, org_img_name = image_label, software = "roxas")
  imgs$image_label[2] <- imgs$image_label[1]
  expect_error(suppressMessages(QWAimages(imgs)), "duplicate")
})

test_that("key_props reads the key columns and their types from the schema", {
  expect_equal(key_props("cells"),
               c(image_label = "string", year = "integer", xpix = "integer", ypix = "integer"))
  expect_equal(names(key_props("roxas_ai")),
               c("image_label", "slide_label", "woodpiece_label", "tree_label", "site_label"))
})

test_that("validate_keys aborts on missing, mistyped, NA and duplicate keys", {
  keys <- c(image_label = "string", year = "integer")
  df <- data.frame(image_label = "IMG1", year = c(2000, 2001))
  expect_identical(validate_keys(df, keys, "rings", unique_by = names(keys)), df)

  expect_error(validate_keys(df["year"], keys, "rings"), "missing key column")
  expect_error(validate_keys(dplyr::mutate(df, year = c(2000.5, 2001)), keys, "rings"),
               "wrong type")
  expect_error(validate_keys(dplyr::mutate(df, year = c(2000, NA)), keys, "rings",
                             hint = "Check the files"),
               "1 row with missing values.*IMG1.*Check the files")
  expect_error(validate_keys(dplyr::mutate(df, year = 2000), keys, "rings", unique_by = names(keys)),
               "2 rows with duplicate")
  expect_no_error(validate_keys(dplyr::mutate(df, year = 2000), keys, "cells"))
})

test_that("check_data_schema reports rule violations with row counts", {
  schema_path <- system.file(schema_rel_path("cells"), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  tbl_props <- get_tbl_props(resolve_schema(schema_obj, schema_path))
  cells <- data.frame(
    image_label = "IMG1", year = c(2000, 2000, 2001), xpix = 1, ypix = 1,
    la = c(10, 20, 30), ew_lw = c("ew", NA, "lw")
  )
  expect_true(check_data_schema(cells, tbl_props, "cells"))

  bad <- cells
  bad$la <- c(-1, -2, 30)
  bad$ew_lw <- c("ew", "xx", "lw")
  bad$year <- c(2000.5, 2000, 2001)
  expect_warning(res <- check_data_schema(bad, tbl_props, "cells"), "failed validation")
  expect_false(res)
  msg <- tryCatch(check_data_schema(bad, tbl_props, "cells", strict = TRUE),
                  error = conditionMessage)
  expect_match(msg, "must be >= 0: `la` \\(2 rows\\)")
  expect_match(msg, "must be equal to one of the allowed values: `ew_lw`.*\\(1 row\\)")
  expect_match(msg, "must be integer: `year`")

  expect_message(check_data_schema(dplyr::mutate(cells, my_col = 1), tbl_props, "cells"),
                 "my_col")
})
